{application, monitor,
    [{description, "Pressure Monitor server App"},
     {vsn, "0.1"},
     {modules, [
                monitor_app,
                monitor_server
                ]},
     {registered, [monitor_server]},
     {applications, [kernel, stdlib, sasl]},
     {mod, {monitor_app, []}}
    ]
}.
