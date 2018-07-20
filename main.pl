:-use_module(library(pce)).
:-use_module(library(pce_style_item)).
:-use_module(library(pce_util)).
:-pce_image_directory('/images').

:-dynamic y/1.
:-dynamic n/1.


:-op(1020, fx, si).
:-op(1015, xfy, entonces).
:-op(1010, xfy, y).
:-op(1005, xfy, o).
:-op(1000, fx, no).

resource(imagen1,image,image('image1.jpg')).
resource(imagen2,image,image('phyton.jpg')).
resource(imagen3,image,image('java.jpg')).
resource(imagen4,image,image('c.jpg')).
resource(imagen5,image,image('cplus.jpg')).
resource(imagen6,image,image('javascrip.jpg')).
resource(imagen7,image,image('csharp.jpg')).
resource(imagen8,image,image('ruby.jpg')).
resource(imagen9,image,image('php.jpg')).
resource(imagen10,image,image('objc.jpg')).



%primera ventana en aparecer y hace la conexion
inicio:-
  new(VentanaPrincipal,dialog('Lenguajes de programacion')),
          new(Etiqueta1,label(nombre,'Tu primer lenguaje de programacion')),
          new(Botondiagnosticar,button('Preguntas' ,message(@prolog,comenzar))),
          new(Botoninf,button('Informacion',message(@prolog,informacion))),
          new(Botonhistorial,button('Registro',message(@prolog,historial))),
          new(Botonsalir,button('Salir',message(VentanaPrincipal,destroy))),
          new(EtiquetaImg,label(nombre2,resource(imagen1))),

          send(Etiqueta1,font,font(times,bold,12)),

          send_list(VentanaPrincipal,append,[Etiqueta1,EtiquetaImg,Botondiagnosticar,Botonhistorial,Botoninf,Botonsalir]),

send(VentanaPrincipal,open).
%main

comenzar:-
  abrir_conexion,
 intro,
 reset_answers,
 find_language(Language),
 describe(Language),
 resultado(Language),
 nl.

%resultado

%informacion del sistema experto
informacion:-new(Inform,dialog('sistema experto para elegir primer lenguaje de programacion')),
          new(Etiquetainf,label(nombre,'\n Jovanny Garcia Zamora \n
Juan Diego Hernandez Solis \n Elliot Mora Ferro \n Edgar de Jesus Median Castro \n Eric Tierrafria Flores \n Hernan Daniel Cortez Jauregui.
          ')),
          new(Botonsalirinf,button('Salir',message(Inform,destroy))),


          send_list(Inform,append,[Etiquetainf,Botonsalirinf]),

  send(Inform,open).

intro :-
  write('Which programming language should I learn first?'), nl,
  write('To answer, input the number shown next to each answer, followed by a dot (.)'), nl, nl.


find_language(Language) :-
  language(Language), !.


% Store user answers to be able to track his progress
:- dynamic(progress/2).


% Clear stored user progress
% reset_answers must always return true; because retract can return either true
% or false, we fail the first and succeed with the second.
reset_answers :-
  retract(progress(_, _)),
  fail.
reset_answers.


% Rules for the knowledge base
language(python) :-
  why(for_my_kids).

language(python) :-
  why(i_dont_know).

language(java) :-
  why(make_money),
  which_platform(doesn_t_matter).

language(cpp) :-
  why(make_money),
  which_platform(gaming).

language(objectivec) :-
  why(make_money),
  which_platform(mobile),
  which_mobile_os(ios).

language(java) :-
  why(make_money),
  which_platform(mobile),
  which_mobile_os(android).

language(python) :-
  why(make_money),
  which_platform(facebook).

language(python) :-
  why(make_money),
  which_platform(google).

language(csharp) :-
  why(make_money),
  which_platform(microsoft).

language(objectivec) :-
  why(make_money),
  which_platform(apple).

language(javascript) :-
  why(make_money),
  which_platform(web),
  web(front_end).

language(csharp) :-
  why(make_money),
  which_platform(web),
  web(back_end),
  want_to_work_for(corporate),
  think_about_microsoft(im_a_fan).

language(java) :-
  why(make_money),
  which_platform(web),
  web(back_end),
  want_to_work_for(corporate),
  think_about_microsoft(not_bad).

language(java) :-
  why(make_money),
  which_platform(web),
  web(back_end),
  want_to_work_for(corporate),
  think_about_microsoft(suck).

language(javascript) :-
  why(make_money),
  which_platform(web),
  web(back_end),
  want_to_work_for(startup),
  try_something_new(yes).

language(python) :-
  why(make_money),
  which_platform(web),
  web(back_end),
  want_to_work_for(startup),
  try_something_new(no),
  favourite_toy(lego).

language(ruby) :-
  why(make_money),
  which_platform(web),
  web(back_end),
  want_to_work_for(startup),
  try_something_new(no),
  favourite_toy(play_doh).

language(php) :-
  why(make_money),
  which_platform(web),
  web(back_end),
  want_to_work_for(startup),
  try_something_new(no),
  favourite_toy(old_ugly).

language(csharp) :-
  why(make_money),
  which_platform(enterprise),
  think_about_microsoft(im_a_fan).

language(java) :-
  why(make_money),
  want_to_work_for(enterprise),
  think_about_microsoft(not_bad).

language(java) :-
  why(make_money),
  want_to_work_for(enterprise),
  think_about_microsoft(suck).

language(python) :-
  why(just_for_fun),
  prefer_to_learn(easy_way).

language(python) :-
  why(just_for_fun),
  prefer_to_learn(best_way).

language(java) :-
  why(just_for_fun),
  prefer_to_learn(harder_way),
  car(auto).

language(c) :-
  why(just_for_fun),
  prefer_to_learn(harder_way),
  car(manual).

language(cpp) :-
  why(just_for_fun),
  prefer_to_learn(hardest_way).

language(python) :-
  why(im_interested),
  prefer_to_learn(easy_way).

language(python) :-
  why(im_interested),
  prefer_to_learn(best_way).

language(java) :-
  why(im_interested),
  prefer_to_learn(harder_way),
  car(auto).

language(c) :-
  why(im_interested),
  prefer_to_learn(harder_way),
  car(manual).

language(cpp) :-
  why(im_interested),
  prefer_to_learn(hardest_way).

language(python) :-
  why(improve_myself),
  prefer_to_learn(easy_way).

language(python) :-
  why(improve_myself),
  prefer_to_learn(best_way).

language(java) :-
  why(improve_myself),
  prefer_to_learn(harder_way),
  car(auto).

language(c) :-
  why(improve_myself),
  prefer_to_learn(harder_way),
  car(manual).

language(cpp) :-
  why(improve_myself),
  prefer_to_learn(hardest_way).


% Questions for the knowledge base

question(why) :-
  write('Why do you want to learn programming?'), nl.

question(which_platform) :-
  write('Which platform/field?'), nl.

question(which_mobile_os) :-
  write('Which OS?'), nl.

question(web) :-
  write('Which "end"?'), nl.

question(want_to_work_for) :-
  write('I want to work for...'), nl.

question(think_about_microsoft) :-
  write('What do you think about Microsoft?'), nl.

question(try_something_new) :-
  write('Do you want to try something new, with huge potential, but less mature?'), nl.

question(favourite_toy) :-
  write('Which one is your favourite toy?'), nl.

question(prefer_to_learn) :-
  write('I prefer to learn things...'), nl.

question(car) :-
  write('Auto or Manual car?'), nl.


% Answers for the knowledge base
answer(for_my_kids) :-
  write('For my kids').

answer(i_dont_know) :-
  write('I don\'t know').

answer(make_money) :-
  write('Make money').

answer(just_for_fun) :-
  write('Just for fun').

answer(im_interested) :-
  write('I\'m interested').

answer(improve_myself) :-
  write('Improve myself').

answer(doesn_t_matter) :-
  write('Doesn\'t matter, I just want $$$').

answer(gaming) :-
  write('3D/Gaming').

answer(mobile) :-
  write('Mobile').

answer(facebook) :-
  write('Facebook').

answer(google) :-
  write('Google').

answer(microsoft) :-
  write('Microsoft').

answer(apple) :-
  write('Apple').

answer(web) :-
  write('Web').

answer(enterprise) :-
  write('Enterprise').

answer(ios) :-
  write('iOS').

answer(android) :-
  write('Android').

answer(front_end) :-
  write('Front-end (web interface)').

answer(back_end) :-
  write('Back-end ("brain" behind a website)').

answer(startup) :-
  write('Startup').

answer(corporate) :-
  write('Corporate').

answer(im_a_fan) :-
  write('I\'m a fan!').

answer(not_bad) :-
  write('Not Bad').

answer(suck) :-
  write('Suck').

answer(yes) :-
  write('Yes').

answer(no) :-
  write('No').

answer(lego) :-
  write('Lego').

answer(play_doh) :-
  write('Play-Doh').

answer(old_ugly) :-
  write('I\'ve an old & ugly toy, but I love it so much!').

answer(easy_way) :-
  write('The easy way').

answer(best_way) :-
  write('The best way').

answer(harder_way) :-
  write('The slightly harder way').

answer(hardest_way) :-
  write('The really hard way (but easier to pick up other languages in the future)').

answer(auto) :-
  write('Auto').

answer(manual) :-
  write('Manual').


% Language descriptions for the knowledge base
describe(python) :-
  new(VentanaPrincipal,dialog('Lenguaje indicado')),
          new(Etiqueta1,label(nombre,'Phyton\nWidely regarded as the best programming language for beginners\nEasiest to learn')),
          new(EtiquetaImg,label(nombre2,resource(imagen2))),

          send(Etiqueta1,font,font(times,bold,12)),

          send_list(VentanaPrincipal,append,[Etiqueta1,EtiquetaImg]),

send(VentanaPrincipal,open).

describe(java) :-
   new(VentanaPrincipal,dialog('Lenguaje indicado')),
          new(Etiqueta1,label(nombre,'Java\nOne of the most in demand & highest paying programming languages\nSlogan: write once, work everywhere')),
          new(EtiquetaImg,label(nombre2,resource(imagen3))),

          send(Etiqueta1,font,font(times,bold,12)),

          send_list(VentanaPrincipal,append,[Etiqueta1,EtiquetaImg]),

send(VentanaPrincipal,open).
describe(c) :-
   new(VentanaPrincipal,dialog('Lenguaje indicado')),
          new(Etiqueta1,label(nombre,'C\nLingua franca of programming language\nOne of the oldest and most widely used language in the world')),
          new(EtiquetaImg,label(nombre2,resource(imagen4))),

          send(Etiqueta1,font,font(times,bold,12)),

          send_list(VentanaPrincipal,append,[Etiqueta1,EtiquetaImg]),

send(VentanaPrincipal,open).

describe(cpp) :-
   new(VentanaPrincipal,dialog('Lenguaje indicado')),
          new(Etiqueta1,label(nombre,'C++\nComplex version of C with a lot more features\nRecommended only if you have a mentor to guide you')),
          new(EtiquetaImg,label(nombre2,resource(imagen5))),

          send(Etiqueta1,font,font(times,bold,12)),

          send_list(VentanaPrincipal,append,[Etiqueta1,EtiquetaImg]),

send(VentanaPrincipal,open).

describe(javascript) :-
   new(VentanaPrincipal,dialog('Lenguaje indicado')),
          new(Etiqueta1,label(nombre,'JavaScript\nMost popular clients-side web scripting language\nA must learn for front-end web developer (HTML and CSS as well)')),
          new(EtiquetaImg,label(nombre2,resource(imagen6))),

          send(Etiqueta1,font,font(times,bold,12)),

          send_list(VentanaPrincipal,append,[Etiqueta1,EtiquetaImg]),

send(VentanaPrincipal,open).


describe(csharp) :-
   new(VentanaPrincipal,dialog('Lenguaje indicado')),
          new(Etiqueta1,label(nombre,'C#\nA popular choice for enterprise to create websites and Windows application using .NET framework\nSimilar to Java in basic syntax and some features')),
          new(EtiquetaImg,label(nombre2,resource(imagen7))),

          send(Etiqueta1,font,font(times,bold,12)),

          send_list(VentanaPrincipal,append,[Etiqueta1,EtiquetaImg]),

send(VentanaPrincipal,open).


describe(ruby) :-
   new(VentanaPrincipal,dialog('Lenguaje indicado')),
          new(Etiqueta1,label(nombre,'Ruby\nMostly known for its popular web framework, Ruby on Rails\nFocuses on getting things done')),
          new(EtiquetaImg,label(nombre2,resource(imagen8))),

          send(Etiqueta1,font,font(times,bold,12)),

          send_list(VentanaPrincipal,append,[Etiqueta1,EtiquetaImg]),

send(VentanaPrincipal,open).
 describe(php) :-
   new(VentanaPrincipal,dialog('Lenguaje indicado')),
          new(Etiqueta1,label(nombre,'PHP\nSuitable for building small and simple sites within a short time frame\nSupported by almost every web hosting services with lower price')),
          new(EtiquetaImg,label(nombre2,resource(imagen9))),

          send(Etiqueta1,font,font(times,bold,12)),

          send_list(VentanaPrincipal,append,[Etiqueta1,EtiquetaImg]),

send(VentanaPrincipal,open).

describe(objectivec) :-
   new(VentanaPrincipal,dialog('Lenguaje indicado')),
          new(Etiqueta1,label(nombre,'ObjetiveC\nPrimary language used by Apple for MacOSX & iOS\nChoose this if you want to focus on developing iOS or OSX apps only')),
          new(EtiquetaImg,label(nombre2,resource(imagen10))),

          send(Etiqueta1,font,font(times,bold,12)),

          send_list(VentanaPrincipal,append,[Etiqueta1,EtiquetaImg]),

send(VentanaPrincipal,open).



% Assigns an answer to questions from the knowledge base
why(Answer) :-
  progress(why, Answer).
why(Answer) :-
  \+ progress(why, _),
  ask(why, Answer, [for_my_kids, i_dont_know, make_money, just_for_fun, im_interested, improve_myself]).

which_platform(Answer) :-
  progress(which_platform, Answer).
which_platform(Answer) :-
  \+ progress(which_platform, _),
  ask(which_platform, Answer, [doesn_t_matter, gaming, mobile, facebook, google, microsoft, apple, web, enterprise]).

which_mobile_os(Answer) :-
  progress(which_mobile_os, Answer).
which_mobile_os(Answer) :-
  \+ progress(which_mobile_os, _),
  ask(which_mobile_os, Answer, [ios, android]).

web(Answer) :-
  progress(web, Answer).
web(Answer) :-
  \+ progress(web, _),
  ask(web, Answer, [front_end, back_end]).

want_to_work_for(Answer) :-
  progress(want_to_work_for, Answer).
want_to_work_for(Answer) :-
  \+ progress(want_to_work_for, _),
  ask(want_to_work_for, Answer, [startup, corporate]).

think_about_microsoft(Answer) :-
  progress(think_about_microsoft, Answer).
think_about_microsoft(Answer) :-
  \+ progress(think_about_microsoft, _),
  ask(think_about_microsoft, Answer, [im_a_fan, not_bad, suck]).

try_something_new(Answer) :-
  progress(try_something_new, Answer).
try_something_new(Answer) :-
  \+ progress(try_something_new, _),
  ask(try_something_new, Answer, [yes, no]).

favourite_toy(Answer) :-
  progress(favourite_toy, Answer).
favourite_toy(Answer) :-
  \+ progress(favourite_toy, _),
  ask(favourite_toy, Answer, [lego, play_doh, old_ugly]).

prefer_to_learn(Answer) :-
  progress(prefer_to_learn, Answer).
prefer_to_learn(Answer) :-
  \+ progress(prefer_to_learn, _),
  ask(prefer_to_learn, Answer, [easy_way, best_way, harder_way, hardest_way]).

car(Answer) :-
  progress(car, Answer).
car(Answer) :-
  \+ progress(car, _),
  ask(car, Answer, [auto, manual]).


% Outputs a nicely formatted list of answers
% [First|Rest] is the Choices list, Index is the index of First in Choices
answers([], _).
answers([First|Rest], Index) :-
  write(Index), write(' '), answer(First), nl,
  NextIndex is Index + 1,
  answers(Rest, NextIndex).


% Parses an Index and returns a Response representing the "Indexth" element in
% Choices (the [First|Rest] list)
parse(0, [First|_], First).
parse(Index, [First|Rest], Response) :-
  Index > 0,
  NextIndex is Index - 1,
  parse(NextIndex, Rest, Response).


% Asks the Question to the user and saves the Answer
ask(Question, Answer, Choices) :-
  question(Question),
  answers(Choices, 0),
  read(Index),
  parse(Index, Choices, Response),
  asserta(progress(Question, Response)),
  Response = Answer.

%METODOS DE BASE DE DATOS

%inserta la informacion a la base de datos
copiar(P,H):-writeln(P),writeln(H),insertar_dato(P,H,_).
%crea una ventana para guardar la informacion
   resultado(G):-writeln(G),new(Resultado,dialog('Informacion')),
          new(Etiqueta,label(nombre,G)),
          new(Texto1,text_item('Nombre del usuario')),
          new(BotonGuardar,button('Guardar',message(@prolog,copiar,Texto1?selection,G))),
          new(Botonsalir2,button('Salir',message(Resultado,destroy))),

          send(Etiqueta,font,font(times,bold,14)),
          send_list(Resultado,append,[Etiqueta,Texto1,BotonGuardar,Botonsalir2]),

  send(Resultado,open).

% --------------------------------------------------------------------------------
% Base de Datos

abrir_conexion:-
    odbc_connect('dbprolog',_,[user(hotel),password('0CC175B9C0F1B6A831C399E269772661'),alias(prolog),open(once)]).
cerrar_conexion:-
    odbc_disconnect('dbprolog').

%Crea una cadena para crear un query
cadenaPH(A,B,C):-concat(A,'","',Z),concat(Z,B,W),concat('INSERT INTO lenguajes(name,lenguaje,date)VALUES("',W,D),concat(D,'",CURDATE())',C).
%inserta informacion
insertar_dato(P,H,X):-cadenaPH(P,H,Cadena),
    odbc_query('prolog',Cadena,affected(X)).
% consulta la informacion de todos los registros no se uso en el sistema
% experto solo para ver como funcionaban las consultas
consultar(F):-
    odbc_query('prolog',
               'SELECT * FROM lenguajes',row(F)).






