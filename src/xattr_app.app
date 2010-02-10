{application, xattr_app,
    [{description, "A port to handle xattr file attributes"},
     {vsn,"0.9"},
     {modules,[xattr_app,xattr_sup,xattr]},
     {registered, [xattr]},
     {applications, [kernel,stdlib]},
     {mod, {xattr_app, []}},
     {env, [
         {extprog, attr_server},
         {timeout, 10000}
         ]}
]}.

         
     
