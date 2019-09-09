module WorkflowDemo.Workflow.Workflows

open Free
open Free.Actions
open WorkflowDemo.Common

let reset = workflow {
    do! setDeviceState Device.Good
    do! cancelDeviceInput ()
    do! resetData ()
    do! clearDeviceMsgs ()
    do! clearControlMsgs ()
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

//TODO: Exercise 3: Implement the branching workflow below
let choice = workflow {
    do! addControlMsg "Workflow not implemented" //Remove this when the workflow is implemented
    
    // Get device input, prompt should ask for path A or B
    // if path A is chosen set the device state to "ugly"
    // if path B is chosen reset the device data
    // if another path is chosen add a control message saying what was entered
}

let recurse = workflow {
    do! addControlMsg "Workflow not implemented (later exercise)"    
}

let fold = workflow {
    do! addControlMsg "Workflow not implemented (later exercise)"    
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