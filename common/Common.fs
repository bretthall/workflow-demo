namespace WorkflowDemo.Common

module Common = 
    
    let pipeName = "workflow-demo"
    
module Device = 
    
    type State = 
        | Good
        | Bad
        | Ugly
    
    type Msg =        
        | SetState of State
        | RequestInput of string
        | CancelInput
        | AddMsg of string

module Control =
    
    type Msg =
        | DataUpdate of int
        | InputReceived of string
        | ServerExit
    
    