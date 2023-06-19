% rule/3 (Id,Head,Conditions)
rule(1,condition(X,'nitrogen'),[manifests(X,symptom('lower leaves','altered color','chlorotic'))]).
rule(2,condition(X,'nitrogen'),[manifests(X,symptom('leaves','early fall','none'))]).
rule(3,condition(X,'nitrogen'),[manifests(X,symptom('roots','increased growth','none'))]).
rule(4,condition(X,'nitrogen'),[manifests(X,symptom('shoot','stunted growth','none'))]).
rule(5,condition(X,'phosphorus'),[manifests(X,symptom('leaves','poor quantity','none'))]).
rule(6,condition(X,'phosphorus'),[manifests(X,symptom('leaves','altered color','dark green'))]).
rule(7,condition(X,'phosphorus'),[manifests(X,symptom('leaves','small size','none'))]).
rule(8,condition(X,'phosphorus'),[manifests(X,symptom('roots','increased growth','none'))]).
rule(9,condition(X,'phosphorus'),[manifests(X,symptom('shoot','stunted growth','none'))]).
rule(10,condition(X,'potassium'),[manifests(X,symptom('lower leaves','altered color','blotchy chlorosis'))]).
rule(11,condition(X,'potassium'),[manifests(X,symptom('leaves','altered color','interveinal chlorosis'))]).
rule(12,condition(X,'potassium'),[manifests(X,symptom('upper leaves','stunted growth','none'))]).
rule(13,condition(X,'sulfur'),[manifests(X,symptom('upper leaves','altered color','chlorotic'))]).
rule(14,condition(X,'sulfur'),[manifests(X,symptom('upper leaves','stunted growth','none'))]).
rule(15,condition(X,'sulfur'),[manifests(X,symptom('leaves','thin size','none'))]).
rule(16,condition(X,'magnesium'),[manifests(X,symptom('lower leaves','altered color','interveinal chlorosis'))]).
rule(17,condition(X,'magnesium'),[manifests(X,symptom('leaves','altered color','chlorotic'))]).
rule(18,condition(X,'magnesium'),[manifests(X,symptom('leaves','altered color','reddish purple'))]).
rule(19,condition(X,'calcium'),[manifests(X,symptom('leaves','deformed','none'))]).
rule(20,condition(X,'calcium'),[manifests(X,symptom('leaves','altered color','dark veins'))]).
rule(21,condition(X,'calcium'),[manifests(X,symptom('upper leaves','spots','brown'))]).
rule(22,condition(X,'calcium'),[manifests(X,symptom('upper leaves','stunted growth','none'))]).
rule(23,condition(X,'calcium'),[manifests(X,symptom('leaves','parachute shape','none'))]).
rule(24,condition(X,'calcium'),[manifests(X,symptom('all','deformed','none'))]).
rule(25,condition(X,'boron'),[manifests(X,symptom('buds','fail to form','none'))]).
rule(26,condition(X,'boron'),[manifests(X,symptom('leaves','irregular lesions','none'))]).
rule(27,condition(X,'boron'),[manifests(X,symptom('shoot','death of growing point','none'))]).
rule(28,condition(X,'boron'),[manifests(X,symptom('upper leaves','altered color','chlorotic'))]).
rule(29,condition(X,'boron'),[manifests(X,symptom('leaves','spots','whitish yellow'))]).
rule(30,condition(X,'boron'),[manifests(X,symptom('stem','stunted growth','none'))]).
rule(31,condition(X,'boron'),[manifests(X,symptom('leaves','deformed','none'))]).
rule(32,condition(X,'boron'),[manifests(X,symptom('buds','deformed','none'))]).
rule(33,condition(X,'chloride'),[manifests(X,symptom('leaves','spots','necrotic'))]).
rule(34,condition(X,'chloride'),[manifests(X,symptom('leaves','spots','chlorotic'))]).
rule(35,condition(X,'chloride'),[manifests(X,symptom('leaves','wilting margins','none'))]).
rule(36,condition(X,'chloride'),[manifests(X,symptom('roots','highly branched growth','none'))]).
rule(37,condition(X,'copper'),[manifests(X,symptom('upper leaves','altered color','chlorotic'))]).
rule(38,condition(X,'copper'),[manifests(X,symptom('all','altered color','brown discoloration'))]).
rule(39,condition(X,'copper'),[manifests(X,symptom('all','stunted growth','none'))]).
rule(40,condition(X,'copper'),[manifests(X,symptom('all','delayed growth','none'))]).
rule(41,condition(X,'copper'),[manifests(X,symptom('all','excessive tillering growth','none'))]).
rule(42,condition(X,'iron'),[manifests(X,symptom('fruits','poor quantity','none'))]).
rule(43,condition(X,'iron'),[manifests(X,symptom('fruits','poor quality','none'))]).
rule(44,condition(X,'iron'),[manifests(X,symptom('upper leaves','altered color','angular chlorosis'))]).
rule(45,condition(X,'iron'),[manifests(X,symptom('roots','altered color','brown'))]).
rule(46,condition(X,'iron'),[manifests(X,symptom('root','smell strange','none'))]).
rule(47,condition(X,'manganese'),[manifests(X,symptom('leaves','withering','none')),manifests(X,symptom('leaves','altered color','brown'))]).
rule(48,condition(X,'manganese'),[manifests(X,symptom('lower leaves','altered color','chlorotic'))]).
rule(49,condition(X,'manganese'),[manifests(X,symptom('leaves','altered color','interveinal chlorosis'))]).
rule(50,condition(X,'manganese'),[manifests(X,symptom('leaves','spots','brown'))]).
rule(51,condition(X,'zinc'),[manifests(X,symptom('upper leaves','altered color','rounded chlorosis'))]).
rule(52,condition(X,'zinc'),[manifests(X,symptom('leaves','altered color','bronze'))]).
rule(53,condition(X,'zinc'),[manifests(X,symptom('leaves','stunted growth','none'))]).
rule(54,condition(X,'zinc'),[manifests(X,symptom('leaves','deformed','none'))]).
rule(55,condition(X,'black spot'),[manifests(X,symptom('leaves','chlorotic','none')),manifests(X,symptom('leaves','early fall','none'))]).
rule(56,condition(X,'black spot'),[manifests(X,symptom('upper side leaves','spots','black,none'))]).
rule(57,condition(X,'leaf spot'),[manifests(X,symptom('leaves','visible spores','none')),manifests(X,symptom('leaves','spots','brown')),manifests(X,symptom('leaves','spots','dark margin'))]).
rule(58,condition(X,'leaf spot'),[manifests(X,symptom('leaves','visible spores','none')),manifests(X,symptom('leaves','spots','tan')),manifests(X,symptom('leaves','spots','dark margin'))]).
rule(59,condition(X,'leaf spot'),[manifests(X,symptom('leaves','visible spores','none')),manifests(X,symptom('leaves','spots','reddish')),manifests(X,symptom('leaves','spots','dark margin'))]).
rule(60,condition(X,'leaf spot'),[manifests(X,symptom('leaves','angular lesions','none'))]).
rule(61,condition(X,'leaf spot'),[manifests(X,symptom('leaves','angular lesions','none')),manifests(X,symptom('leaves','altered color','yellowish outline'))]).
rule(62,condition(X,'leaf spot'),[manifests(X,symptom('leaves','circular lesions','none'))]).
rule(63,condition(X,'leaf spot'),[manifests(X,symptom('leaves','circular lesions','none')),manifests(X,symptom('leaves','altered color','yellowish outline'))]).
rule(64,condition(X,'leaf spot'),[manifests(X,symptom('leaves','necrotic lesions','none'))]).
rule(65,condition(X,'leaf spot'),[manifests(X,symptom('leaves','necrotic lesions','none')),manifests(X,symptom('leaves','altered color','yellowish outline'))]).
rule(66,condition(X,'powdery mildew'),[manifests(X,symptom('all','white powdery substance','none'))]).
rule(67,condition(X,'downy mildew'),[manifests(X,symptom('upper side lower leaves','spots','pale green'))]).
rule(68,condition(X,'downy mildew'),[manifests(X,symptom('upper side lower leaves','spots','yellow'))]).
rule(69,condition(X,'downy mildew'),[manifests(X,symptom('lower side leaves','white cotton like downy substance','none'))]).
rule(70,condition(X,'downy mildew'),[manifests(X,symptom('lower side leaves','greysh cotton like downy substance','none'))]).
rule(71,condition(X,'downy mildew'),[manifests(X,symptom('lower side leaves','grayish fuzzy looking spores','none'))]).
rule(72,condition(X,'blight'),[manifests(X,symptom('leaves','altered color','chlorotic'))]).
rule(73,condition(X,'blight'),[manifests(X,symptom('all','spot','none'))]).
rule(74,condition(X,'blight'),[manifests(X,symptom('all','withering','none'))]).
rule(75,condition(X,'blight'),[manifests(X,symptom('all','death of plant tissue','none'))]).
rule(76,condition(X,'canker'),[manifests(X,symptom('leaves','small holes','none'))]).
rule(77,condition(X,'canker'),[manifests(X,symptom('bark','dead patches','none'))]).
rule(78,condition(X,'shot hole'),[manifests(X,symptom('leaves','spots','tan'))]).
rule(79,condition(X,'shot hole'),[manifests(X,symptom('leaves','spots drop out','none'))]).
rule(80,condition(X,'shot hole'),[manifests(X,symptom('leaves','shot holes','none'))]).
rule(81,condition(X,'shot hole'),[manifests(X,symptom('leaves','spots','purplish'))]).
rule(82,condition(X,'shot hole'),[manifests(X,symptom('fruits','spots','red'))]).
rule(83,condition(X,'shot hole'),[manifests(X,symptom('fruits','spots','red')),manifests(X,symptom('fruits','clear gummy substance','none'))]).
rule(84,condition(X,'shot hole'),[manifests(X,symptom('fruits','spots','purplish'))]).
rule(85,condition(X,'shot hole'),[manifests(X,symptom('fruits','spots','purplish')),manifests(X,symptom('fruits','clear gummy substance','none'))]).
rule(86,condition(X,'early blight'),[manifests(X,symptom('leaves','spots','dark brown'))]).
rule(87,condition(X,'early blight'),[manifests(X,symptom('leaves','spots','black'))]).
rule(88,condition(X,'early blight'),[manifests(X,symptom('stem','spots','black'))]).
rule(89,condition(X,'early blight'),[manifests(X,symptom('fruits','sunken spot','black'))]).
rule(90,condition(X,'early blight'),[manifests(X,symptom('fruits','leathery spot','black'))]).
rule(91,condition(X,'early blight'),[manifests(X,symptom('fruits','large spots','black'))]).
rule(92,condition(X,'late blight'),[manifests(X,symptom('lower leaves','irregular water soaked blotches','greenish black'))]).
rule(93,condition(X,'late blight'),[manifests(X,symptom('stem','irregular water soaked blotches','greenish black'))]).
rule(94,condition(X,'late blight'),[manifests(X,symptom('lower leaves','irregular water soaked blotches','greenish black')),manifests(X,symptom('fruits','irregular water soaked blotches','greenish black'))]).
rule(95,condition(X,'late blight'),[manifests(X,symptom('stem','irregular water soaked blotches','greenish black')),manifests(X,symptom('fruits','irregular water soaked blotches','greenish black'))]).
rule(96,condition(X,'grey mold'),[manifests(X,symptom('all','fuzzy mold','grey'))]).
rule(97,condition(X,'grey mold'),[manifests(X,symptom('stem','water soaked spots','none'))]).
rule(98,condition(X,'grey mold'),[manifests(X,symptom('flower','water soaked spots','none'))]).
rule(99,condition(X,'grey mold'),[manifests(X,symptom('lower leaves','water soaked spots','none'))]).
rule(100,condition(X,'verticilium wilt'),[manifests(X,symptom('branches','altered color','chlorotic'))]).
rule(101,condition(X,'verticilium wilt'),[manifests(X,symptom('branches','wilting','none'))]).
rule(102,condition(X,'verticilium wilt'),[manifests(X,symptom('branches','dying','none'))]).
rule(103,condition(X,'apthids'),[manifests(X,symptom('all','sticky honeydew','none'))]).
rule(104,condition(X,'apthids'),[manifests(X,symptom('all','deformed','none'))]).
rule(105,condition(X,'apthids'),[manifests(X,symptom('soil','husks','white'))]).
rule(106,condition(X,'apthids'),[manifests(X,symptom('soil','husks','grey'))]).
rule(107,condition(X,'apthids'),[manifests(X,symptom('leaves','random chlorosis','none'))]).
rule(108,condition(X,'thrips'),[manifests(X,symptom('leaves','mottled','none'))]).
rule(109,condition(X,'thrips'),[manifests(X,symptom('leaves','streaking','none'))]).
rule(110,condition(X,'thrips'),[manifests(X,symptom('leaves','altered color','brown'))]).
rule(111,condition(X,'spider mites'),[manifests(X,symptom('all','sticky webbing','none'))]).
rule(112,condition(X,'spider mites'),[manifests(X,symptom('leaves','mottled','none')),manifests(X,symptom('leaves','spots','brown'))]).
rule(113,condition(X,'scale insects'),[manifests(X,symptom('all','sticky honeydew','none'))]).
rule(114,condition(X,'white flies'),[manifests(X,symptom('leaves','flies','white')),manifests(X,symptom('leaves','altered color','chlorotic'))]).
rule(115,condition(X,'white flies'),[manifests(X,symptom('leaves','flies','white')),manifests(X,symptom('leaves','dry','none'))]).
rule(116,condition(X,'cutworms'),[manifests(X,symptom('leaves','cutworms under leaves','none'))]).
rule(117,condition(X,'cutworms'),[manifests(X,symptom('all','cutworms under debris','none'))]).
rule(118,condition(X,'fungus gnats'),[manifests(X,symptom('all','small flies','black'))]).
rule(119,condition(X,'mealy bugs'),[manifests(X,symptom('all','stunted growth','none'))]).
rule(120,condition(X,'mealy bugs'),[manifests(X,symptom('all','withering','none'))]).
rule(121,condition(X,'mealy bugs'),[manifests(X,symptom('leaves','altered color','chlorotic'))]).
rule(122,condition(X,'mealy bugs'),[manifests(X,symptom('lower side leaves','clustering cottony','none'))]).
rule(123,condition(X,'mealy bugs'),[manifests(X,symptom('leaves','altered color','chlorotic')),manifests(X,symptom('leaves','early fall','none'))]).
rule(124,condition(X,'mealy bugs'),[manifests(X,symptom('all','dry','none'))]).
rule(125,condition(X,'mealy bugs'),[manifests(X,symptom('all','sticky honeydew','none'))]).
rule(126,condition(X,'wet'),[manifests(X,reading(humidity,high))]).
rule(127,condition(X,'dry'),[manifests(X,reading(humidity,low))]).
rule(128,condition(X,'hot'),[manifests(X,reading(temperature,high))]).
rule(129,condition(X,'cold'),[manifests(X,reading(temperature,low))]).
rule(130,problem(X,'climate'),[condition(X,'wet')]).
rule(131,problem(X,'climate'),[condition(X,'dry')]).
rule(132,problem(X,'climate'),[condition(X,'hot')]).
rule(133,problem(X,'climate'),[condition(X,'cold')]).
rule(134,problem(X,'infestation'),[condition(X,'apthids')]).
rule(135,problem(X,'infestation'),[condition(X,'thrips')]).
rule(136,problem(X,'infestation'),[condition(X,'spider mites')]).
rule(137,problem(X,'infestation'),[condition(X,'scale insects')]).
rule(138,problem(X,'infestation'),[condition(X,'white flies')]).
rule(139,problem(X,'infestation'),[condition(X,'cutworms')]).
rule(140,problem(X,'infestation'),[condition(X,'fungus gnats')]).
rule(141,problem(X,'infestation'),[condition(X,'mealy bugs')]).
rule(142,problem(X,'disease'),[condition(X,'black spot')]).
rule(143,problem(X,'disease'),[condition(X,'leaf spot')]).
rule(144,problem(X,'disease'),[condition(X,'powdery mildew')]).
rule(145,problem(X,'disease'),[condition(X,'downy mildew')]).
rule(146,problem(X,'disease'),[condition(X,'blight')]).
rule(147,problem(X,'disease'),[condition(X,'canker')]).
rule(148,problem(X,'disease'),[condition(X,'shot hole')]).
rule(149,problem(X,'disease'),[condition(X,'early blight')]).
rule(150,problem(X,'disease'),[condition(X,'late blight')]).
rule(151,problem(X,'disease'),[condition(X,'grey mold')]).
rule(152,problem(X,'disease'),[condition(X,'verticilium wilt')]).
rule(153,problem(X,'nutrient deficiency'),[condition(X,'nitrogen')]).
rule(154,problem(X,'nutrient deficiency'),[condition(X,'phosphorus')]).
rule(155,problem(X,'nutrient deficiency'),[condition(X,'potassium')]).
rule(156,problem(X,'nutrient deficiency'),[condition(X,'sulfur')]).
rule(157,problem(X,'nutrient deficiency'),[condition(X,'magnesium')]).
rule(158,problem(X,'nutrient deficiency'),[condition(X,'calcium')]).
rule(159,problem(X,'nutrient deficiency'),[condition(X,'boron')]).
rule(160,problem(X,'nutrient deficiency'),[condition(X,'chloride')]).
rule(161,problem(X,'nutrient deficiency'),[condition(X,'copper')]).
rule(162,problem(X,'nutrient deficiency'),[condition(X,'iron')]).
rule(163,problem(X,'nutrient deficiency'),[condition(X,'manganese')]).
rule(164,problem(X,'nutrient deficiency'),[condition(X,'zinc')]).
rule(165,issue(X,'abiotic'),[problem(X,'climate')]).
rule(166,issue(X,'abiotic'),[problem(X,'nutrient deficiency')]).
rule(167,issue(X,'biotic'),[problem(X,'infestation')]).
rule(168,issue(X,'biotic'),[problem(X,'disease')]).