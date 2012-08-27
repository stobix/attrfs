{application, reporter,
    [{description, "A reporting utility, for debugging and whatnot."},
        {vsn,"1.0"},
        {modules,[reporter,reporter_sup]},
        {registered,[reporter]},
        {applications,[kernel,stdlib]},
        {mod,{reporter,[]}},
        {env,[]}]}.

