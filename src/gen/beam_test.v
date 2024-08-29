module gen

fn test_generate_beam_sum_two_integer() {
	source := '1 + 2'
	expected :=
		'
{module, script}.  %% version = 0
{exports, [{module_info,0},{module_info,1},{run,0}]}.
{attributes, []}.
{labels, 7}.
{function, run, 0, 2}.
  {label,1}.
    {func_info,{atom,script},{atom,run},0}.
  {label,2}.
    {gc_bif,' +
		',{f,0},1,[{x,0},{integer,2}],{x,0}}.
		{gc_bif,' +
		',{f,0},1,[{x,0},{integer,2}],{x,0}}.
    return.
{function, module_info, 0, 4}.
  {label,3}.
    {func_info,{atom,script},{atom,module_info},0}.
  {label,4}.
    {move,{atom,script},{x,0}}.
    {call_ext_only,1,{extfunc,erlang,get_module_info,1}}.
{function, module_info, 1, 6}.
  {label,5}.
    {func_info,{atom,script},{atom,module_info},1}.
  {label,6}.
    {move,{x,0},{x,1}}.
    {move,{atom,script},{x,0}}.
    {call_ext_only,2,{extfunc,erlang,get_module_info,2}}.\n'
	assert expected == generate_beam(source)!
}
