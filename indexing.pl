
% indexing
% Starts the indexing for every rule
indexing_init :-
	rule(RuleID,Head,_,_,_),
	not(irule(RuleID,Head,_)),
	assert(irule(RuleID,Head,[])),
	index(RuleID,Head),
	fail.
indexing.

indexing :-
	rule(RuleID,Head,_,_,_),
	irule(RuleID,Head,_),
	index(RuleID,Head),
	fail.
indexing.

% Given a rule Head with CurrentRuleID starts the check on whether it is present in other rules' conditions
index(CurrentRuleID,Head) :-  
	rule(RuleID,_,Conditions,_,_),
	CurrentRuleID \== RuleID,
    rule_check(CurrentRuleID,RuleID,Head,Conditions),
	fail.
index(_,_).