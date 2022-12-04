{application , 'crafting_challenge' , 

 [

	{description , "crafting software coding challenge for erlang engineer"},

	{vsn , "0.1"},

	{modules , ['crafting_challenge' , 'crafting_challenge_app' , 'crafting_challenge_sup' ,

		    'crafting_challenge_server' , 'crafting_challenge_utils'

		   ]},

	{applications , [kernel , stdlib , jsone]},

	{mod , {crafting_challenge_app , []}}

 ]

}.	
	 
