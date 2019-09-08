module WorkflowDemo.Workflow.Workflows

open Free
open Free.Actions
open WorkflowDemo.Common

let reset = workflow {
    do! setDeviceState Device.Good
    do! addControlMsg "Set device state"
    do! clearDeviceMsgs ()
    do! addControlMsg "Cleared device msgs"
    do! cancelDeviceInput ()
    do! addControlMsg "cancelled input"
    do! resetData ()
    do! addControlMsg "reset date"
}

let wait = workflow {
    do! setDeviceState Device.Good
    do! addDeviceMsg "Waiting for 5 seconds"
    do! wait 5<seconds>
    do! setDeviceState Device.Bad
    let! curData = getCurrentData ()
    let targetData = curData + 5
    do! addDeviceMsg (sprintf "Waiting for data = %d" targetData)
    let! finalData = waitForData targetData
    do! addDeviceMsg (sprintf "final data = %d" finalData)
    do! setDeviceState Device.Ugly
    do! addDeviceMsg "Waiting for input"
    let! input = getDeviceInput "Waiting for input"
    do! addDeviceMsg (sprintf "Got input = %s" input)
    do! setDeviceState Device.Good
}

let choice = workflow {
    let! input = getDeviceInput "Path A or B?"
    if input = "A" then
        do! addControlMsg "Chose path A"
    else if input = "B" then
        do! addControlMsg "Chose path B"
    else
        do! addControlMsg (sprintf "Chose other path: %s" input)
}

let recurseDeterminant =
    let rec step index = workflow {
        if index <= 10 then 
            do! addControlMsg (sprintf "repeat number %d" index)
            return! step (index + 1)
        else
            return ()
    }
    step 1
    
let recurseIndeterminant =
    let rec getInput index = workflow {
        let! input = getDeviceInput "Input (\"stop\" to stop)"
        if input.ToLower () <> "stop" then
            do! addControlMsg (sprintf "Input %d: %s" index input)
            return! getInput (index + 1)
        else
            return index - 1
    }
    workflow {
        let! num = getInput 1
        do! addControlMsg (sprintf "Got %d inputs" num)
    }
    
let recurseWithFold = workflow {
    let! num = 
        (0, [Device.Good; Device.Bad; Device.Ugly]) ||> fold (
            fun s t ->
                workflow {
                    do! setDeviceState t
                    do! Free.wait 5<seconds>
                    return (s + 1)           
                }
        )
    do! addControlMsg (sprintf "Did %d state changes" num)
}

type Workflow =
    | Reset
    | Wait
    | Choice
    | RecurseDeterminant
    | RecurseIndeterminant
    | RecurseWithFold
    
let getProgram workflow =
    match workflow with
    | Reset -> reset
    | Wait -> wait
    | Choice -> choice
    | RecurseDeterminant -> recurseDeterminant
    | RecurseIndeterminant -> recurseIndeterminant
    | RecurseWithFold -> recurseWithFold