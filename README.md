# COSI 135 Final Project: VP Ellipsis

In this project, I explore how to model VP ellipsis using a continuation-based approach to anaphora, 
modelled in lambda-calculus and implemented in Haskell. 
I follow Van Eijck and Unger (2010) in their general approach towards handling context with continuations, and draw on De Groote (2006), Barker and Shan (2014) and Asher and Pogodalla (2010) for details of the continuized meanings of our constituents. 

The full report is attached to this repository as `report.pdf`.

## VP Ellipsis

VP ellipsis is a quintessential context-dependency phenomenon. It captures fact that when we want to repeat a previously mentioned event, we need not spell out the same words again, we can instead refer to it simply with _did too_:

* Dorothy laughed. Alice did too.
* Alice helped Atreyu. Dorothy did too.
* Dorothy has defeated a wizard. Alice has too. 
* Alice cheers. Dorothy will too.
* Atreyu loves his sword. Little Mook does too.

Because certain actions are already given by the context, we can replace them with the simple _did (has, will) too_ when we want to repeat them. In other words, VP ellipsis provides an excellent way to study what is (or should be) in the context: if we can omit it, the context must contain it. There are other kinds of ellipsis such as NP ellipsis, as well as other kinds of anaphora such as pronominal anaphora, all of which provide further `windows' on what is in the context. Here, we focus on VP ellipsis because it shows that the context does not just contains objects from the model such as entities or predicates; instead, it must contain at least some semantic representations.

VP ellipsis requires both syntactic and semantic analysis to explain all its behaviours; Cann, Kempson, and Gregoromichelaki (2009) gives an overview. For the purpose of this project, we will not engage with its syntactic puzzles, but rather concentrate on interpreting its meaning in discourse and its contribution to the contents of the context, using a simple grammar.

As shown by the examples, VP ellipsis does not simply "copy and paste" the meaning of the previous VP into the next: the theme of the verb may change. The wizard that Dorothy defeated need not be the wizard that Alice defeated. Little Mook most likely loves his own sword, not Atreyu's. (Both readings are however possible.) Further, the tense can change: the ellipsis sentence contains an auxiliary which carries the new time at which the event happens. Alice is cheering now but Dorothy will only cheer later. (Perhaps they are supporting opposing teams.) In this project, we will take apart these cases one by one to determine exactly what we must store in the context.