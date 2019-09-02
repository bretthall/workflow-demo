module WorkflowDemo.Workflow.Workflows

open Free
open WorkflowDemo.Common

type Workflow =
    | Reset
    | Test
    | Wait
    
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

let getProgram workflow =
    match workflow with
    | Reset -> reset
    | Test -> test
    | Wait -> wait