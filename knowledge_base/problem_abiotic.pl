% status_problem/2
status_problem(abiotic, wet) :- reading(humidity, high).
status_problem(abiotic, dry) :- reading(humidity, low).
status_problem(abiotic, hot) :- reading(temperature, high).
status_problem(abiotic, cold) :- reading(temperature, low).