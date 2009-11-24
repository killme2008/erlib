{application, pressure_monitor,
    [{description, "Pressure Monitor server App"},
     {vsn, "0.1"},
     {modules, [
                pressure_monitor_app,
                monitor_server,
                socket_server
                ]},
     {registered, [monitor_server]},
     {applications, [kernel, stdlib, sasl]},
     {mod, {pressure_monitor_app, [8001]}}
    ]
}.
