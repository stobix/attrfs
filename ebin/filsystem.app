{ application, filsystem, 
   [ {mod, {filsystem_app, []}},
     {description, "Ett applikationstest, en filserver..."},
     {vsn, "0.0.1"},
     {registered, [me,filsystemet]},
     {applications, [kernel,stdlib]},
     {modules, [filsystem,filsystem_sup,filsystem_app]},
     {env, [{test,"hej123"}]}
]}.
