%% \documentclass{article}
%% \begin{document}

%% \title{App Ctrl: Attempt at QuickCheck Specification}
%% \maketitle

%% \section{Introduction}

%% A cluster controller is responsible for managing a collection of
%% applications running on a collection of processors. It responds to
%% events such as changes in a processor's status, by starting,
%% stopping, and moving applications around, according to the
%% constraints in a static {\em configuration} of the controller. This
%% document is an attempt to specify the behaviour of such a cluster
%% controller, previously "ClusterWare Light", now "App Ctrl".

%% The document as a whole is a literal Erlang module, using the
%% QuickCheck API.

-module(app_ctrl_eqc).
-include_lib("eqc/include/eqc.hrl").
-compile([export_all, nowarn_export_all]).

%% \section{Notations}

%% This section introduces some useful notations for use later.

%% We can generate non-empty lists as follows,
nonempty(G) ->
    ?SUCHTHAT(L,G,L/=[]).

%% and sorted lists by sorting.

sorted(G) ->
    ?LET(L,G,lists:sort(L)).

%% We compare sets and bags by sorting.

set_eq(S1,S2) ->
    lists:usort(S1) == lists:usort(S2).

bag_eq(S1,S2) ->
    lists:sort(S1) == lists:sort(S2).

%% Check that Xs contains Ys (Ys is a subset of Xs)
contains(Xs,Ys) ->
    Ys--Xs == [].

%% We represent relations as lists of pairs; we may need their domain,
%% range and inverse:

dom(R) ->
    remove_duplicates([X || {X,_} <- R]).

ran(R) ->
    dom(inverse(R)).

inverse(R) ->
    [{Y,X} || {X,Y} <- R].

%% The {\em image} of a set through a relation is the set of values
%% related to elements of the set:

image(Set,R) ->
    [Y || X <- Set,
	  {X1,Y} <- R,
	  X == X1].

%% We may convert relations to and from "functions" that associate
%% elements in the domain with lists of elements in the range.

group([]) ->
    [];
group([{X,Y}|R]) ->
    [{X,[Y|[Y1 || {X1,Y1} <- R,
		  X==X1]]}
     |group([{X1,Y1} || {X1,Y1} <- R,
			X/=X1])].

ungroup([]) ->
    [];
ungroup([{X,Ys}|F]) ->
    [{X,Y} || Y <- Ys] ++ ungroup(F).

%% We define a generator for mappings:

mapping(DomG,RanG) ->
    ?LET(Dom,ulist(DomG),
	 [{X,RanG} || X <- Dom]).

%% Now we can state inverse relationships between \verb!group! and
%% \verb!ungroup!.

prop_group_ungroup() ->
    ?FORALL(F,mapping(int(),nonempty(ulist(int()))),
	    group(ungroup(F)) == F).

prop_ungroup_group() ->
    ?FORALL(R,list({int(),int()}),
	    bag_eq(ungroup(group(R)),R)).

%% We sometimes need to specify that the elements of a list are {\em different}:

different(L) ->
    lists:usort(L) == lists:sort(L).

%% We can create lists of different elements by removing duplicates from lists with
%% duplicates.

remove_duplicates(L) ->
    lists:foldr(fun(H,T)->[H|lists:delete(H,T)] end,[],L).

prop_different_remove_duplicates() ->
    ?FORALL(L,list(int()),
	    different(remove_duplicates(L))).

prop_remove_duplicates_different() ->
    ?FORALL(L,list(int()),
	    ?IMPLIES(different(L),remove_duplicates(L)==L)).

%% We generate lists of different elements using remove\_duplicates:

ulist(G) ->
    ?LET(L,list(G),remove_duplicates(L)).

%% \section{Basic Concepts}

%% \subsection{Applications}

%% The purpose of a cluster controller is to control the execution of
%% applications. An application is a top-level OTP application,
%% represented in this document by an atom. For example, the following
%% are applications:

-define(applications,[sip,diameter,megaco,snmp,inets,perf,b2bua,b2bua_sb]).

%% Suitable test data can be generated as follows:

application() ->
    elements(?applications).

%% \subsection{R\^oles}

%% A processor playing a certain r\^ole will usually run a set of
%% related applications. During the lifetime of a product, the r\^oles
%% of processors are actually more stable than the particular set of
%% applications that implement that r\^ole. Therefore, we make the
%% notion of a r\^ole explicit, and assign r\^oles to processors,
%% rather than applications. 

%% R\^oles are named by atoms, for example:

-define(role_names,[basic,oam,active,standby]).

role_name() ->
    elements(?role_names).

%% They correspond to a set of applications. The specification of the
%% r\^oles in use is a part of the configuration of the cluster
%% controller, represented as a mapping from role names to lists of
%% applications. Each application may appear in at most one r\^ole.

valid_roles(Roles) ->
    different(dom(Roles)) andalso different(ran(ungroup(Roles))).

%% We can generate valid r\^ole assignments by inverting a mapping from
%% applications to role names.

roles() ->
    ?LET(AppRoles,mapping(application(),role_name()),
	 group(inverse(AppRoles))).

prop_valid_roles() ->
    ?FORALL(Roles,roles(),valid_roles(Roles)).

%% \section{The Meaning of Layout}

%% The layout field of a group specifies how roles should be assigned
%% to slots, based on which of the slots are currently filled. We assume

%% \begin{itemize}
%% \item \verb!Slots! is a list of the group slots:
valid_slots(Slots) ->
    different(Slots).
%% \item \verb!SlotMembers! is a mapping from slots to processors
valid_slot_members(Slots,SlotMembers) ->
    contains(Slots,dom(SlotMembers)) andalso different(ran(SlotMembers)).
%% filling those slots (a list of slot-processor pairs) 
%% \item \verb!Layout! is a layout specification as per Ulf's slides.
%% \end{itemize}

%% Then the {\em meaning} of the layout is a relation between r\^oles
%% and members, defined as follows:

layout_meaning(Slots,SlotMembers,Layout) ->
    [{Role,Proc} 
     || Elem <- Layout,
	{Role,Proc} <- layout_element_meaning(Slots,SlotMembers,Elem)].

%% That is, the meaning of a layout is a combination of the meanings
%% of its elements. \verb!on_each! just assigns the r\^ole to each
%% active slot:
layout_element_meaning(_Slots,SlotMembers,{on_each,Roles}) ->
    [{Role,Proc} || Role <- Roles,
		    Proc <- ran(SlotMembers)];
%% R\^oles can be explicitly assigned to slots---provided the slot is
%% filled.
layout_element_meaning(_Slots,SlotMembers,{Slot,Roles}) 
  when is_atom(Slot)->
    [{Role,Proc} || Proc <- image([Slot],SlotMembers),
		    Role <- Roles];
%% The \verb!if_active! property is interesting: note that the value
%% of this property, according to Ulf's slides, is just a special case
%% of a layout! We can therefore generalise the property to allow an
%% arbitrary layout specification---and at the same time simplify the
%% meaning.
layout_element_meaning(Slots,SlotMembers,{{if_active,N},Layout}) ->
    case length(SlotMembers) of
	N ->
	    layout_meaning(Slots,SlotMembers,Layout);
	_ ->
	    []
    end.

%% When is a layout {\em valid}? It seems to me we should require that
%% no r\^ole is assigned to the same slot more than once---in other words
valid_layout_meaning(Slots,SlotMembers,Layout) ->
    different(layout_meaning(Slots,SlotMembers,Layout)).

%% However, I wonder whether this is entirely satisfactory? Note that
%% this meaning function {\em completely specifies} the r\^oles that
%% should be assigned to each slot. This means there is little scope
%% for the cluster controller to {\em vary} the assignment depending
%% on load, for example. Essentially the only way the cluster
%% controller is allowed to influence the r\^ole assignment is by
%% choosing which slots to fill with members (assuming the group is
%% not packed). Perhaps the controller should be allowed more freedom?

%% For example, I wonder whether instead of specifying for each slot,
%% a list of r\^oles to be run there, might it make sense to specify
%% {\em for each r\^ole, a list of slots where it might run?}. Such a
%% list could be interpreted in priority order (run on the first slot
%% that is filled), or in non-deterministic order (run on one of these
%% slots, choice left to the cluster controller), or in another
%% specified order (run on the least heavily loaded slot, for
%% example). This kind of specification might be useful for any group,
%% with any number of filled slots, as opposed to the \verb!if_active!
%% properties which are really only sensible for mated pairs.

%% \end{document}


%% First important concept: that of a processor. I believe the
%% following fields are fixed, i.e. the node and type of a processor
%% never change.

-record(processor,
	{name,    % an atom, used to identify processor in the API
	 node,    % the Erlang node running the processor--fixed?
	 type}).  % e.g. master, worker, undefined--what's this for?

processor_name() ->
    elements([p1,p2,p3,p4,p5,p6,p7,p8,p9,p10]).

processor_type() ->
    elements([master,worker,undefined]).

processor() ->
    ?LET(Name,processor_name(),
	 #processor{name = Name,
		    node = list_to_atom(atom_to_list(Name)++"@debian"),
		    type = processor_type()}).

%% A processor may be up or down. Of course, this varies with time.

-record(processors,
	{up,      % a list of the processors which are up
	 down}).  % a list of the processors which are down

valid_processors(#processors{up=Up,down=Down}) ->
    different([P#processor.name || P <- Up++Down]).

processors() ->
    ?LET({PreUp,PreDown},{list(processor()),list(processor())},
	 #processors{up = filter_duplicate_names([],PreUp),
		     down = filter_duplicate_names(
			      [P#processor.name || P <- PreUp],
			      PreDown)}).

filter_duplicate_names(_Names,[]) ->
    [];
filter_duplicate_names(Names,[P|Ps]) ->
    case lists:member(P#processor.name,Names) of
	true ->
	    filter_duplicate_names(Names,Ps);
	false ->
	    [P|filter_duplicate_names([P#processor.name|Names],Ps)]
    end.

prop_valid_processors() ->
    ?FORALL(Procs,processors(),valid_processors(Procs)).
		   





%% The purpose of clware is to decide what runs where--i.e. produce an
%% assignment of applications to processors. The same application may
%% be running on several different processors... but I assume each
%% application may run only once per processor.
%%
%% Perhaps simpler is to assign *roles* to processors, thus
%% guaranteeing that all the applications in the same role are
%% assigned to the same processor. So an Assignment will be a list of
%% pairs of a processor name, and a list of role names.
%%
%% Presumably, all of the processors assigned roles must be up.
%%
%% QUESTION: Must every role be assigned to some processor?

%% \begin{verbatim}
%% valid_assignment(Processors,Roles,Assignment) ->
%%     lists:all(fun({PName,Rs}) ->
%% 		      (lists:keymember(PName,
%% 				       #processor.name,
%% 				       Processors#processors.up)
%% 		       orelse Rs==[])
%% 			  andalso lists:all(fun(R) -> 
%% 						    lists:keymember(R,
%% 								    #role.name,
%% 								    Roles)
%% 					    end,
%% 					    Rs)
%% 			  andalso different(Rs)
%% 	      end,
%% 	      Assignment).
%%
%% assignment(Processors,Roles) ->
%%     [{P#processor.name,
%%       case lists:member(P,Processors#processors.up) of
%% 	  true ->
%% 	      ulist(elements([R#role.name || R <- Roles]));
%% 	  false ->
%% 	      []
%%       end}
%%      || P <- Processors#processors.up++Processors#processors.down].
%%
%% prop_valid_assigment() ->
%%     ?FORALL({Processors,Roles},{processors(),roles()},
%% 	    ?FORALL(Assignment,assignment(Processors,Roles),
%% 		    valid_assignment(Processors,Roles,Assignment))).
%% \end{verbatim}

%% The job of clware is then to produce a valid assignment of roles to
%% processors. GROUPS tell clware how to do that, I guess. So given a
%% set of processors, and a list of groups, clware should produce a
%% valid assignment. Perhaps the previous assignment is also relevant,
%% to avoid moving applications unnecessarily. I guess the assignment
%% must also satisfy the groups, somehow, but I understand groups
%% poorly.

%% I think it's a bit strange for unfilled slots to have the value [],
%% rather than an atom--none or undefined, perhaps.

%% I don't understand what it means for a group not to be unique. The
%% group members seem to be node names, and they'll be the same for
%% each "instance" of the group, so what is the point of replicating
%% the group?

%% What is the point of unfilled processor slots in a group's members?
%% As I understand it, groups are static configuration data--so if a
%% group member is undefined, then it will always be undefined!

%% Can a processor be in more than one group? Must each processor be
%% in exactly one group?

%% Let me try to define when an assignment on a given set of
%% processors satisfies a group. A group is represented as on Ulf's
%% slides, except that:
%% * I ignore the possibility to name slots.
%% * I assume all groups are unique.

%% This definition has not been tested.

satisfies_group(Processors,Assignment,Group={_,Props}) ->
    % Which active processors belong to the group?
    Active = [P || P <- Processors#processors.up,
		   lists:member(P#processor.node,
				getp(members,Props))],
    % All processors must have the correct type
    TypesOK = lists:all(fun(P) ->
				lists:member(P#processor.type,
					     getp(types,Props))
				    orelse getp(types,Props) == []
			end,
			Active),
    % The "on_each" roles must be assigned to each processor
    Layout = getp(layout,Props),
    OnEachOK = lists:all(fun(P) ->
				 contains(getp(P,Assignment),
					  getp(on_each,Layout))
			 end,
			 Active),
    % The "if_active" clauses must be satisfied
    IfActiveOK =
	case [RoleSpec || {if_active,N,RoleSpec} <- Layout,
			  N == length(Active)] of
	    [] ->
		% no role spec given for this number of active processors
	        % is this an error or not?
		true;
	    [RoleSpec] ->
		% a list of pairs of roles and lists of processors
		% some processors may not be up
		% I guess each processor that is up must be assigned
		% the roles whose list it appears in
		lists:all(
		  fun(P) ->
			  Roles = [R || {R,PNames} <- RoleSpec,
					lists:member(P#processor.name,PNames)],
			  contains(getp(P,Assignment),Roles)
		  end,
		  Active)
	end,
    % I don't understand the significance of the packed or reserve properties.
    TypesOK andalso OnEachOK andalso IfActiveOK.

%% What are base\_a and base\_b in the examples? The members of group
%% base are called a and b, as far as I can see.

%% The later slides make it appear as though groups are NOT static,
%% that the group members can come and go. But then if the group
%% members are not known statically, what sense do the lists of
%% processor names in an if\_active property make? Might these stand
%% for different processors dynamically? If so, how is the connection
%% made?

%% Auxiliary functions

%% Get a property.
getp(Name,PL) ->
    proplists:get_value(Name,PL).

