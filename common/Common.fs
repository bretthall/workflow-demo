namespace WorkflowDemo.Common

module Device = 

    type Msg = 
        | Bar of int

module Control =
    
    type Msg = 
        | Foo of int
        | ServerExit
    
module Common = 
    
    let pipeName = "workflow-demo"
    