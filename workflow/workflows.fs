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

//TODO: Exercise 1: Fill in the following workflow
let wait = workflow {
    do! addControlMsg "Workflow not implemented" //remove this when you've implemented the below

    // set the device state to "good"
    // wait 5 seconds
    // set the device state to "bad"
    // wait for the data to increment 5 times (you'll need to get the current data value)
    // add a device message saying what the data value is at the end of the wait
    // set the device state to "ugly"
    // get input from the device
    // add a control message saying what the input is
    // set the device state to "good"
}

let choice = workflow {
    do! addControlMsg "Workflow not implemented (later exercise)"    
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