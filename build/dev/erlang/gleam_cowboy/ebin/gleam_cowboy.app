{application, gleam_cowboy, [
    {vsn, "0.7.0"},
    {applications, [cowboy,
                    gleam_erlang,
                    gleam_http,
                    gleam_otp,
                    gleam_stdlib]},
    {description, "Run Gleam HTTP services with the Cowboy web server"},
    {modules, []},
    {registered, []}
]}.
