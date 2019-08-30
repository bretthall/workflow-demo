module WorkflowDemo.Workflow.Workflows

open Free
open WorkflowDemo.Common

type Workflow =
    | Reset
    | Test
    
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

let test = workflow {
    do! setDeviceState Device.Bad
    do! addDeviceMsg "Testing 1 2 3"
    do! addControlMsg "test done"
}
let getProgram workflow =
    match workflow with
    | Reset -> reset
    | Test -> test