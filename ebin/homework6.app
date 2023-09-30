{application, 'homework6', [
	{description, "New project"},
	{vsn, "0.1.0"},
	{modules, ['homework6','homework6_app','homework6_sup']},
	{registered, [homework6_sup]},
	{applications, [kernel,stdlib]},
	{optional_applications, []},
	{mod, {homework6_app, []}},
	{env, []}
]}.