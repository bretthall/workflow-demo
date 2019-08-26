module WorkflowDemo.Workflow.Workflows

open Free
open WorkflowDemo.Common

type Workflow =
    | Reset
    
let reset = workflow {
    do! setDeviceState Device.Good
    do! setControlState ""
    do! clearControlMsgs ()
    do! clearDeviceMsgs ()
    do! cancelDeviceInput ()
    do! resetData ()
}
