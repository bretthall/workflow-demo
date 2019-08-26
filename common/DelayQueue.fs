module WorkflowDemo.Common.DelayQueue

[<Measure>]
type ms

let delayQueue (delay: float<ms>) action = MailboxProcessor<'msg>.Start (fun inbox ->
    let rec loop (stopwatch: System.Diagnostics.Stopwatch) =
        async {                            
            let! msg = inbox.Receive ()
            let remain = int (float delay - stopwatch.Elapsed.TotalMilliseconds)
            if remain > 0 then
                do! Async.Sleep remain
            action msg
            stopwatch.Restart ()
            return! loop stopwatch
        }
    loop (System.Diagnostics.Stopwatch.StartNew ())
)
