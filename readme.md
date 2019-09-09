# Workflow Demo

This is demo code for the talk [Using Free Monads to Simplify Workflows](https://www.openfsharp.org/speakers/2019/brett-hall/) at [Open F# 2019](https://www.openfsharp.org). 

## Building

[.Net Core](https://dotnet.microsoft.com/download) 2.1 or newer is required. To build simply do `dotnet build`. The first time this is done you will need an internet connection so that Nuget packages can be installed.

## Running

There are three programs that can be run:

* device: A _device_ that the workflow will control.
* control: Runs workflows that control a device.
* test: Unit tests for workflows.

Each of them can be run by using `dotnet run -p <project>/<project>.fsproj` from the root of the source tree (`<project>` is one of `device`, `control`, or `test`).

Note that `control` won't do anything until `device` is started. Once you start a program the terminal that it is started from will be tied up until that program exits.

