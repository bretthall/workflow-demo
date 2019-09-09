module WorkflowDemo.Workflow.Workflows

open Free
open Free.Actions
open WorkflowDemo.Common

let reset = workflow {
    do! setDeviceState Device.Good
    do! cancelDeviceInput ()
    do! clearDeviceMsgs ()
    do! clearControlMsgs ()
    //Exercise 6: converted resetData to resetDataWithMsg (also moved to end so the message doesn't get cleared)
    do! resetDataWithMsg ()
}

let wait = workflow {
    // set the device state to "good"
    do! setDeviceState Device.Good
    // wait 5 seconds
    do! wait 5<seconds>
    // set the device state to "bad"
    do! setDeviceState Device.Bad
    // wait for the data to increment 5 times (you'll need to get the current data value)
    let! curData = getCurrentData ()
    let targetData = curData + 5
    let! finalData = waitForData targetData
    // add a device message saying what the data value is at the end of the wait
    do! addDeviceMsg (sprintf "final data = %d" finalData)
    // set the device state to "ugly"
    do! setDeviceState Device.Ugly
    // get input from the device
    let! input = getDeviceInput "Waiting for input"
    // add a control message saying what the input is
    do! addControlMsg (sprintf "Got input = %s" input)
    // set the device state to "good"
    do! setDeviceState Device.Good
}

let choice = workflow {
    // Get device input, prompt should ask for path A or B
    let! input = getDeviceInput "Path A or B?"
    if input = "A" then
        // if path A is chosen set the device state to "ugly"
        do! setDeviceState Device.Ugly
    else if input = "B" then
        // if path B is chosen reset the device data
        do! resetData ()
    else
        // if another path is chosen add a control message saying what was entered
        do! addControlMsg (sprintf "Chose other path: %s" input)
}

let recurse = 
    // Make a recursive workflow that asks for input until the input is "stop".
    let rec getInput index = workflow {
        let! input = getDeviceInput "Input (\"stop\" to stop)"
        if input.ToLower () <> "stop" then
            // A control message containing the input should be added for each input received.
            do! addControlMsg (sprintf "Input %d: %s" index input)
            return! getInput (index + 1)
        else
            return index - 1
    }
    workflow {
        let! num = getInput 1
        // When "stop" is received a device message should be added saying how many inputs were received.     
        do! addDeviceMsg (sprintf "Got %d inputs" num)
    }

let fold = workflow {
    // Use fold to iterate the good, bad, and ugly device states setting each one in the device
    // waiting 5 seconds after each change. While doing the fold count how many changes are made
    // and report that value in a control message when the fold is done.
    let! num = 
        (0, [Device.Good; Device.Bad; Device.Ugly]) ||> fold (
            fun s t ->
                workflow {
                    do! setDeviceState t
                    do! Free.Actions.wait 5<seconds>
                    return (s + 1)           
                }
        )
    do! addControlMsg (sprintf "Did %d state changes" num)    
}

type Workflow =
    | Reset
    | Wait
    | Choice
    | Recurse
    | Fold
    
let getProgram workflow =
    match workflow with
    | Reset -> reset
    | Wait -> wait
    | Choice -> choice
    | Recurse -> recurse
    | Fold -> fold