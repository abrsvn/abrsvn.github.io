#Lexical Semantics -- Wordnet

from __future__ import division
import nltk, re, pprint


#WordNet is a semantically-oriented dictionary of English, similar to a traditional thesaurus but with a richer structure.
#-- 155,287 words and 117,659 synonym sets

# Consider (1a) and (1b) -- they differ only wrt the fact that the word `motorcar' in (1a) is replaced by `automobile' in (1b).

#(1) a. Benz is credited with the invention of the motorcar.
#    b. Benz is credited with the invention of the automobile.

# The meaning of the sentences is pretty much the same.
# Given that everything else in the sentences is the same, we can conclude that the words `motorcar' and `automobile' have the same meaning, i.e. they are synonyms.

#We can explore these words with the help of WordNet.

from nltk.corpus import wordnet as wn
wn.synsets('motorcar')

#`motorcar' has just one possible meaning and it is identified as car.n.01, the first noun sense of `car'.

#The entity car.n.01 is called a synset, or "synonym set": a collection of synonymous words (or "lemmas").

wn.synset('car.n.01').lemma_names

#Each word of a synset can have several meanings, e.g., `car' can also signify a train carriage, a gondola or an elevator car.

for synset in wn.synsets('car'):
    print synset.lemma_names


for synset in wn.synsets('machine'):
    print synset.lemma_names


#But we are only interested in the single meaning that is common to all words of the synset car.n.01.

wn.synset('car.n.01').lemma_names

#Synsets also come with a prose definition and some example sentences:

wn.synset('car.n.01').definition
wn.synset('car.n.01').examples


#Although definitions help humans to understand the intended meaning of a synset, the words of the synset are often more useful for programs.
#-- that is, we identify the various meanings of a single lexical item by the items it is synonymous with

#--`motorcar' has only one synset / meaning

for synset in wn.synsets('motorcar'):
    print synset.lemma_names

#-- `auto' has only one synset / meaning -- the same as `motorcar'

for synset in wn.synsets('auto'):
    print synset.lemma_names

#-- `car' has 5 synsets / meanings

for synset in wn.synsets('car'):
    print synset.lemma_names, '\n'

#-- `machine' has 8 synsets / meanings

for synset in wn.synsets('machine'):
    print synset.lemma_names, '\n'

for synset in wn.synsets('machine'):
    print synset.lemma_names, synset.definition, '\n'


#More examples: `dish', `murder', `newspaper'

for synset in wn.synsets('dish'):
    print synset.lemma_names
for synset in wn.synsets('dish'):
    print synset.lemma_names, synset.definition, "\n", synset.examples, "\n"

for synset in wn.synsets('murder'):
    print synset.lemma_names, synset.definition, "\n", synset.examples, "\n"

for synset in wn.synsets('newspaper'):
    print synset.lemma_names, synset.definition, "\n", synset.examples, "\n"

for synset in wn.synsets('set'):
    print synset.lemma_names, synset.definition, "\n", synset.examples, "\n"
len(wn.synsets('set'))

#WordNet synsets correspond to abstract concepts, and they don't always have corresponding words in English.

#These concepts are linked together in a hierarchy. Some concepts are very general, such as `Entity', `State', `Event' -- called unique beginners or root synsets. Others, such as `gas guzzler' and `hatchback', are much more specific.

#For example, given a concept like `motorcar', we can look at the concepts that are more specific -- the (immediate) hyponyms

motorcar = wn.synset('car.n.01')
motorcar.hyponyms()


# Or at the concepts that are less specific -- the (immediate) hypernyms

motorcar.hypernyms()

#... and further hypernyms

wn.synset('motor_vehicle.n.01').hypernyms()
wn.synset('self-propelled_vehicle.n.01').hypernyms()
wn.synset('wheeled_vehicle.n.01').hypernyms()

#... this better explored by tracking the full path or paths from a synset to a root hypernym

motorcar.root_hypernyms()

paths = motorcar.hypernym_paths()
len(paths)
[synset.name for synset in paths[0]]
[synset.name for synset in paths[1]]



#More lexical relations

#Hypernyms and hyponyms are called lexical relations because they relate one synset to another.
#-- these two relations navigate up and down the "is-a" hierarchy.

# Other important ways to navigate the WordNet network:
#--  from items to their components, i.e., their meronyms

wn.synset('tree.n.01').part_meronyms()
wn.synset('tree.n.01').substance_meronyms()

#-- from items to the things they are contained in, i.e., their holonyms

wn.synset('tree.n.01').member_holonyms()



#Consider for example the word `mint'

for synset in wn.synsets('mint', wn.NOUN):
    print synset.name + ':', synset.definition + "\n"

wn.synset('mint.n.04').part_holonyms()
wn.synset('mint.n.04').substance_holonyms()


#There are also relationships between verbs. For example, the act of walking involves the act of stepping, so walking entails stepping. Some verbs have multiple entailments:

wn.synset('walk.v.01').entailments()
wn.synset('eat.v.01').entailments()
wn.synset('tease.v.03').entailments()


#Antonymy is defined as a relation between words / lemmas:

wn.lemma('supply.n.02.supply').antonyms()
wn.lemma('rush.v.01.rush').antonyms()
wn.lemma('horizontal.a.01.horizontal').antonyms()



#Syntax (Context Free Grammars)

from __future__ import division
import nltk, re, pprint

#Syntactic / structural ambiguity
#-- consider the sentence `I shot an elephant in my pajamas'

groucho_grammar = nltk.parse_cfg("""
S -> NP VP
PP -> P NP
NP -> Det N | Det N PP | 'I'
VP -> V NP | VP PP
Det -> 'an' | 'my'
N -> 'elephant' | 'pajamas'
V -> 'shot'
P -> 'in'
""")

sent = ['I', 'shot', 'an', 'elephant', 'in', 'my', 'pajamas']
parser = nltk.ChartParser(groucho_grammar)
trees = parser.nbest_parse(sent)
for tree in trees:
    print tree

trees[0].draw()
trees[1].draw()


#A simple CFG

grammar1 = nltk.parse_cfg("""
S -> NP VP
VP -> V NP | V NP PP
PP -> P NP
V -> "saw" | "ate" | "walked"
NP -> "John" | "Mary" | "Bob" | "I" | Det N | Det N PP
Det -> "a" | "an" | "the" | "my"
N -> "man" | "dog" | "cat" | "telescope" | "park"
P -> "in" | "on" | "by" | "with"
""")

#A production like VP -> V NP | V NP PP is an abbreviation for the two productions VP -> V NP and VP -> V NP PP

cp = nltk.ChartParser(grammar1)

sent = "Mary saw Bob".split()
trees = cp.nbest_parse(sent)
len(trees)
for tree in trees:
    print tree
trees[0].draw()

sent = "the dog saw a man in the park".split()
trees = cp.nbest_parse(sent)
len(trees)
for tree in trees:
    print tree
trees[0].draw()
trees[1].draw()

sent = "I saw a man with a telescope".split()
trees = cp.nbest_parse(sent)
len(trees)
for tree in trees:
    print tree
trees[0].draw()
trees[1].draw()

grammar2 = nltk.data.load('file:mygrammar.cfg')

#-- make sure that you put a .cfg suffix on the filename, and that there are no spaces in the string 'file:mygrammar.cfg'.

#-- you can check what productions are currently in the grammar with the command:

for p in grammar2.productions():
    print p

#-- if the command print tree produces no output, this is probably because your sentence sent is not admitted by your grammar

cp = nltk.ChartParser(grammar2)

sent = "Mary saw Bob".split()
trees = cp.nbest_parse(sent)
len(trees)
for tree in trees:
    print tree
trees[0].draw()


#-- grammatical categories cannot be combined with lexical items on the righthand side of the same production, e.g., a production such as PP -> 'of' NP is disallowed
# -- multi-word lexical items are not allowed on the righthand side of a production, e.g., instead of writing NP -> 'New York', we need to write something like NP -> 'New_York'


#The L0E grammar (Dowty et al)

grammar3 = nltk.data.load('file:Dowty_et_al.cfg')

for p in grammar3.productions():
    print p

cp = nltk.ChartParser(grammar3)

sent = "Hank hates Liz".split()
trees = cp.nbest_parse(sent)
len(trees)
print trees[0]
trees[0].draw()

sent = "Liz snores and Hank hates Liz".split()
trees = cp.nbest_parse(sent)
len(trees)
trees[0].draw()

sent = "Liz snores and Hank hates Liz or Sadie is-boring".split()
trees = cp.nbest_parse(sent)
len(trees)
trees[0].draw()
trees[1].draw()


#Scaling Up

#We have only considered "toy grammars".

# Can the approach be scaled up to cover large fragments of natural languages? How hard would it be to construct such a set of productions by hand?

#In general, the answer is: very hard.

#Even if we allow ourselves to use various formal devices that give much more succinct representations of grammar productions, it is still extremely difficult to keep control of the complex interactions between the many productions required to cover the major constructions of a language.

#In other words, it is hard to modularize grammars so that one portion can be developed independently of the other parts.

#This in turn means that it is difficult to distribute the task of grammar writing across a team of linguists.

#Another difficulty is that as the grammar expands to cover a wider and wider range of constructions, there is a corresponding increase in the number of analyses which are admitted for any one sentence.

# In other words, AMBIGUITY INCREASES WITH COVERAGE.

#Despite these problems, some large collaborative projects have achieved interesting and impressive results in developing rule-based grammars for several languages, e.g.:
#-- the Lexical Functional Grammar (LFG) Pargram project
#-- the Head-Driven Phrase Structure Grammar (HPSG) LinGO Matrix framework
#-- the Lexicalized Tree Adjoining Grammar XTAG Project



#Treebanks and grammars

#The Penn Treebank corpus: annotation of naturally-occuring text for linguistic structure.
#-- skeletal parses showing rough syntactic and semantic information -- a bank of linguistic trees
#-- part-of-speech tags
#-- and for the Switchboard corpus of telephone conversations, dysfluency annotation

#NLTK comes with 10% of the Penn Treebank corpus

from nltk.corpus import treebank

treebank.fileids()
len(treebank.fileids())

trees = treebank.parsed_sents('wsj_0001.mrg')
len(trees)
print trees[0]
trees[0].draw()
print trees[1]
trees[1].draw()


#We can use this data to help develop a grammar.
#-- for example, we can use a simple filter to find verbs that take sentential complements
#-- assuming we already have a production of the form VP -> Vs S, this information enables us to identify particular verbs that would be included in the expansion of Vs

def filter(tree):
    child_nodes = [child.node for child in tree if isinstance(child, nltk.Tree)]
    return (tree.node == 'VP') and ('S' in child_nodes)

trees = [subtree for tree in treebank.parsed_sents() for subtree in tree.subtrees(filter)]
len(trees)

print trees[200]
trees[200].draw()
print trees[537]
trees[537].draw()
print trees[781]
trees[781].draw()
print trees[1007]
trees[1007].draw()


#Pernicious Ambiguity

#Unfortunately, as the coverage of the grammar increases and the length of the input sentences grows, the number of parse trees grows rapidly.

#To see this, consider a simple example.
#-- the word `fish' is both a noun and a verb
#-- we can make up the sentence `fish fish fish'
#-- this sentence means that fish like to fish for other fish (try the same sentence with `police')

# Here is a toy grammar for `fish' sentences.

grammar4 = nltk.parse_cfg("""
S -> N V N
N -> N Sbar
Sbar -> N V
N -> 'fish'
V -> 'fish'
""")

#We can try parsing a longer sentence:
#-- `fish fish fish fish fish'
#-- among other things, this sentence means that fish that other fish fish are in the habit of fishing fish themselves

# Let's see how many possible trees we have.

cp = nltk.ChartParser(grammar4)

sent = "fish fish fish fish fish".split()
trees = cp.nbest_parse(sent)
len(trees)
trees[0].draw()
trees[1].draw()


# As the length of this sentence goes up (3, 5, 7, 9, 11, ...) we get the following numbers of parse trees: 1, 2, 5, 14, 42 ... That is, the number of parse trees increases dramatically.

#Let's test this for the sentence of length 11.

sent = "fish fish fish fish fish fish fish fish fish fish fish".split()
trees = cp.nbest_parse(sent)
len(trees)
trees[21].draw()

#The average length of sentences in the Wall Street Journal section of Penn Treebank: 23.
#How many possible trees do you think there are for a `fish' sentence of length 23?

sent = ["fish"] * 23
trees = cp.nbest_parse(sent)
len(trees)
trees[20222].draw()

#So much for structural ambiguity.

#How about lexical ambiguity?

#As soon as we try to construct a broad coverage grammar, we are forced to make lexical entries highly ambiguous for their part of speech.
#-- in a toy grammar, `a' is only a determiner, `dog' is only a noun, and `runs' is only a verb.
#-- in a broad coverage grammar, `a' is also a noun (`part a'), `dog' is also a verb (meaning to follow closely), and `runs' is also a noun (`ski runs').

#Thus, a parser for a broad-coverage grammar will be overwhelmed with ambiguity.


#Weighted grammars and probabilistic parsing algorithms have provided an effective solution to these problems.

#Why would we thing that the notion of grammaticality could be gradient?

#Consider the verb `give':
#-- it requires both a direct object (the thing being given) and an indirect object (the recipient)
#-- these complements can be given in either order

#(2) a. Kim gave a bone to the dog.
#    b. Kim gave the dog a bone.

#"Prepositional dative" form: the direct object appears first, followed by a prepositional phrase containing the indirect object.

#"Double object" form: the indirect object appears first, followed by the direct object.

# In the above example, either order is acceptable. However, if the indirect object is a pronoun, there is a strong preference for the double object construction:

#(3) a. *Kim gives the heebie-jeebies to me. (prepositional dative)
#    b. Kim gives me the heebie-jeebies.(double object)

#Using the Penn Treebank sample, we can examine all instances of prepositional dative and double object constructions involving `give'


def give(t):
    return t.node == 'VP' and len(t) > 2 and t[1].node == 'NP' and (t[2].node == 'PP-DTV' or t[2].node == 'NP') and ('give' in t[0].leaves() or 'gave' in t[0].leaves())

def sent(t):
    return ' '.join(token for token in t.leaves() if token[0] not in '*-0')

def print_node(t, width):
    output = "%s %s: %s / %s: %s" % (sent(t[0]), t[1].node, sent(t[1]), t[2].node, sent(t[2]))
    if len(output) > width:
        output = output[:width] + "..."
    print output

for tree in nltk.corpus.treebank.parsed_sents():
    for t in tree.subtrees(give):
        print_node(t, 72)


#We observe a strong tendency for the shortest complement to appear first.

#However, this does not account for a form like `give NP: federal judges / NP: a raise', where animacy may play a role



#A probabilistic context free grammar (or PCFG) is a context free grammar that associates a probability with each of its productions.

#It generates the same set of parses for a text that the corresponding context free grammar does, and assigns a probability to each parse.

#The probability of a parse generated by a PCFG is simply the product of the probabilities of the productions used to generate it.

#The simplest way to define a PCFG: a sequence of weighted productions, where weights appear in brackets.

grammar5 = nltk.parse_pcfg("""
S -> N VP [1.0]
VP -> TV N [0.4]
VP -> IV [0.3]
VP -> DatV N N [0.3]
TV -> 'saw' [1.0]
IV -> 'ate' [1.0]
DatV -> 'gave' [1.0]
N -> 'telescopes' [0.8]
N -> 'Jack' [0.2]
""")
print grammar5


#It is sometimes convenient to combine multiple productions into a single line, e.g., VP -> TV N [0.4] | IV [0.3] | DatV N N [0.3].

#This makes it easier to check that all productions with a given left-hand side have probabilities that sum to 1.

#PCFG grammars impose this constraint so that the trees generated by the grammar form a probability distribution.

#The parse trees includes probabilities now:

vp = nltk.ViterbiParser(grammar5)

sent = 'Jack saw telescopes'.split()
tree = vp.parse(sent)
print tree
tree.draw()

#Since parse trees are assigned probabilities, it no longer matters that there may be a huge number of possible parses for a given sentence. A parser will be responsible for finding the most likely parse or parses.



#Semantics

from __future__ import division
import nltk, re, pprint


#Propositional logic

# sentential connectives, a.k.a. boolean operators
nltk.boolean_ops()

lp = nltk.LogicParser()

formula = lp.parse('-(Liz_snores & Hank_hates_Liz)')
formula
print formula

lp.parse('Liz_snores & Hank_hates_Liz')
lp.parse('Liz_snores | (Hank_hates_Liz -> Liz_hates_Hank)')
lp.parse('Liz_snores <-> --Liz_snores')


#Interpretation, a.k.a. valuation

val = nltk.Valuation([('Liz_snores', True), ('Hank_hates_Liz', True), ('Liz_hates_Hank', False)])

val['Hank_hates_Liz']

dom = set([])
g = nltk.Assignment(dom)

m = nltk.Model(dom, val)

#Every model comes with an evaluate() method, which will determine the semantic value of logical expressions.

print m.evaluate('Hank_hates_Liz', g)
print m.evaluate('Liz_snores & Hank_hates_Liz', g)
print m.evaluate('Liz_snores & Liz_hates_Hank', g)
print m.evaluate('Hank_hates_Liz -> Liz_hates_Hank', g)
print m.evaluate('Liz_snores | (Hank_hates_Liz -> Liz_hates_Hank)', g)



#First-order logic


#Syntax of first-order logic

#We break sentences into predicates and arguments / terms.

#-- terms: individual variables (basically, pronouns) and individual constants (basically, names)
#-- predicates: `walk' is a unary / 1-place predicate, `see' is a binary / 2-place predicate etc.


#It is often helpful to inspect the syntactic structure of expressions of first-order logic and the usual way of doing this is to assign types to expressions.

# Following the tradition of Montague grammar, we use two basic types:
#-- e is the type of expressions like (proper) names that denote entities
#-- t is the type of formulas / sentences, i.e., expressions that denote truth values

#Given these two basic types, we can form complex types for function expressions
# -- <e,t> is the type of expressions that denote functions from entities to truth values, namely 1-place predicates
# -- <e,<e,t>> is the type of expressions that denote functions from entities to functions from entities truth values, namely 2-place predicates


#The LogicParser can be invoked so that it carries out type checking:

import nltk
tlp = nltk.LogicParser(type_check=True)
parsedSent = tlp.parse('walk(angus)')
parsedSent.argument
parsedSent.argument.type
parsedSent.function
parsedSent.function.type


#Why do we see <e,?> at the end of this example?
#-- the type-checker will try to infer as many types as possible
#-- but in this case, it has not managed to fully specify the type of `walk' because its result type is unknown.

#We want `walk' to be of type <e,t> -- so we need to specify this. This specification is sometimes called a `signature':

sig = {'walk': '<e,t>'}
parsed = tlp.parse('walk(angus)', sig)
parsed.function.type

#A binary predicate has type <e,<e,t>>, i.e., it combines first with an argument of type e to make a unary predicate. Then it combines with another argument to form a sentence.
#But for readability, we represent binary predicates as combining with their two arguments simultaneously
#-- e.g., `see(angus, cyril)' and not `see(cyril)(angus)'


#In first-order logic, arguments of predicates can also be individual variables x, y, z etc.
#-- individual variables are similar to personal pronouns like `he', `she' and `it', in that we need to know about the context of use in order to figure out their denotation.

#Consider the sentence below:
#(4) He disappeared.

#-- one way of interpreting the pronoun in (4) is by pointing to a relevant individual in the local context.
#-- another way is to supply a textual antecedent for the pronoun, for example by uttering (5) prior to (4).

#(5) Cyril is a bad dog.
#(4) He disappeared.

#-- we say that `he' is coreferential with the noun phrase Cyril
#-- in this case, (4) semantically equivalent to (6) below:

#(6) Cyril disappeared.

#Variables in first-order logic correspond to pronouns:

#(7) a. He is a dog and he disappeared.
#     b. dog(x) & disappear(x)


#By placing an existential quantifier `exists x' ('for some x'), we can form new sentences in which these variables are interpreted as dependent pronouns

#(8) exists x.(dog(x) & disappear(x))
# (9) a. There is a dog and he disappeared.
#     b. There is a dog that disappeared.
#     c. A dog disappeared.

#In addition to the existential quantifier,we have a universal quantifier `all x'

#(10) all x.(dog(x) -> disappear(x))
# (11) a. For any entity x, if it is a dog, it disappeared.
#      b. All entities that are dogs disappeared.
#      c. Every dog disappeared.


#The parse() method of NLTK's LogicParser returns objects of class Expression.
#-- for each expression, we can determine the set of variables that are free in it.

lp = nltk.LogicParser()
lp.parse('dog(cyril)').free()
lp.parse('dog(x)').free()
lp.parse('own(angus, cyril)').free()
lp.parse('exists x.dog(x)').free()
lp.parse('exists x.own(x, y)').free()
lp.parse('all y. (dog(y) -> exists x. (person(x) & own(x, y)))').free()



#Semantics of first-order logic

#Given a first-order logic language L, a model M for L is a pair <D, Val>:
#-- D is a nonempty set called the domain of the model; it consists of the entities / `pebbles' in our model
#-- Val is a function called the valuation function which assigns appropriate semantic values to the basic expressions of L


dom = set(['b', 'o', 'c'])
v = """
bertie => b
olive => o
cyril => c
boy => {b}
girl => {o}
dog => {c}
walk => {o, c}
see => {(b, o), (c, b), (o, c)}
"""
val = nltk.parse_valuation(v)
print val

#draw a picture for this valuation / model!


#The first-order logic counterpart of a context of use is a variable assignment.

#-- this is a mapping from individual variables to entities in the domain

g = nltk.Assignment(dom, [('x', 'o'), ('y', 'c')])
g
print g


#We can evaluate atomic formulas:
#-- we create a model
#-- we call the evaluate() method to compute the truth value


m = nltk.Model(dom, val)
m.evaluate('see(cyril, bertie)', g)
m.evaluate('see(olive, y)', g)
m.evaluate('see(y, x)', g)
m.evaluate('see(bertie, olive) & boy(bertie) & -walk(bertie)', g)


g
g.purge()
g

m.evaluate('exists x.(girl(x) & walk(x))', g)
m.evaluate('girl(x) & walk(x)', g.add('x', 'o'))

#Satisfiers

fmla1 = lp.parse('girl(x) | boy(x)')
m.satisfiers(fmla1, 'x', g)
fmla2 = lp.parse('girl(x) -> walk(x)')
m.satisfiers(fmla2, 'x', g)
fmla3 = lp.parse('walk(x) -> girl(x)')
m.satisfiers(fmla3, 'x', g)


m.evaluate('all x.(girl(x) -> walk(x))', g)
m.satisfiers(fmla2, 'x', g)

m.evaluate('all x.(girl(x) | boy(x))', g)
m.satisfiers(fmla1, 'x', g)



#The lambda-calculus


#lambda (symbolized as \ from here on) is a binding operator, just as the first-order logic quantifiers are.
#-- if we have an open formula such as (12a), then we can bind the variable x with the lambda operator, as shown in (12b)
#-- informally, the resulting expression is interpreted as shown in (12c) and (12d)

#(12) a. (walk(x) & chew_gum(x))
#     b. \x.(walk(x) & chew_gum(x))
#     c. the set of entities x such that x walks and x chews gum
#     d. the entities that both walk and chew gum

lp = nltk.LogicParser()
e = lp.parse(r'\x.(walk(x) & chew_gum(x))')
e
e.free()
print lp.parse(r'\x.(walk(x) & chew_gum(y))')


#Another example

#(13) a. \x.(walk(x) & chew_gum(x)) (gerald)
#     b. gerald is one of the entities x that both walk and chew gum
#     c. gerald both walks and chews gum
#     d. walk(gerald) & chew_gum(gerald)

e = lp.parse(r'\x.(walk(x) & chew_gum(x)) (gerald)')
print e
print e.simplify()


#More examples

#(14) a. \x.(dog(x) & own(y, x))
#     b. give paraphrase here

#(15) a. \x.\y.(dog(x) & own(y, x))
#     b. give paraphrase here

print lp.parse(r'\x.\y.(dog(x) & own(y, x))(cyril)').simplify()

print lp.parse(r'\x.\y.(dog(x) & own(y, x))(cyril, angus)')
print lp.parse(r'\x.\y.(dog(x) & own(y, x))(cyril, angus)').simplify()


#alpha conversion (a.k.a. renaming bound variables)

e1 = lp.parse('exists x.P(x)')
print e1

e2 = e1.alpha_convert(nltk.sem.Variable('z'))
print e2
e1 == e2

print e2.alpha_convert(nltk.sem.Variable('y17'))



#Quantified NPs

#Consider the simple sentences in (16a) and (17a) below:

#(16) a. A dog barks.
#     b. exists x.(dog(x) & bark(x))

#(17) a. No dog barks.
#     b. -(exists x.(dog(x) & bark(x)))

#-- we want to assign a separate meaning to the NPs `a dog' and `no dog' in much the same way in which we assign a separate meaning to `Cyril' in `Cyril barks'
#-- furthermore, whatever meaning we assign to these NPs, we want the final truth conditions for the English sentences in (16a) and (17a) to be the ones assigned to the first-order formulas in (16b) and (17b), respectively

#We do this by lambda-abstraction over higher-order variables -- in particular, over property variables P, P2 etc.


#-- a dog
lp.parse(r'(\P.exists x.(dog(x) & P(x)))')
#-- barks
lp.parse(r'\y.(bark(y))')
#-- a dog barks
S1 = lp.parse('(\P.exists x.(dog(x) & P(x)))(\y.(bark(y)))')
print S1
print S1.simplify()


#-- no dog
lp.parse(r'(\P.-(exists x.(dog(x) & P(x))))')
#-- barks
lp.parse(r'\y.(bark(y))')
#-- no dog barks
S2 = lp.parse('(\P.-(exists x.(dog(x) & P(x))))(\y.(bark(y)))')
print S2
print S2.simplify()


#We can provide a translation for the determiners `a' and `no' by adding another lambda abstraction.
#We exemplify with `no' only.

#-- no
lp.parse(r'(\P2.\P.-(exists x.(P2(x) & P(x))))')
#-- dog
lp.parse(r'\z.(dog(z))')
#-- no dog
NP1 = lp.parse(r'(\P2.\P.-(exists x.(P2(x) & P(x))))(\z.(dog(z)))')
print NP1
print NP1.simplify()
#-- barks
lp.parse(r'\y.(bark(y))')
#-- no dog barks
S3 = lp.parse('((\P2.\P.-(exists x.(P2(x) & P(x))))(\z.(dog(z)))(\y.(bark(y))))')
print S3
print S3.simplify()



#Transitive verbs with quantified NPs as direct objects

#This is the simplest translation for the transitive verb `see'
lp.parse(r'\x2.\x1.see(x1,x2)')

#It works great for direct objects that are proper names, e.g., `Cyril saw Angus'
S4 = lp.parse(r'(\x2.\x1.see(x1,x2)(angus))(cyril)')
print S4
print S4.simplify()

#It doesn't work for direct objects that are quantified NPs, e.g., `Cyril saw no boy'

#-- the final formula for this sentence should be:
print lp.parse(r'-(exists x.(boy(x) & see(cyril,x)))')
#-- instead, we get this
S5 = lp.parse(r'(\x2.\x1.see(x1,x2)((\P.-(exists x.(boy(x) & P(x))))))(cyril)')
print S5
print S5.simplify()

#We can give a more general translation in which a transitive verb lambda-abstracts over a higher-order variable that has the same type as quantified NPs

#-- saw
TV1 = lp.parse(r'(\Q.\x1.Q(\x2.see(x1,x2)))')
print TV1
#-- no boy
lp.parse(r'(\P.-(exists x.(boy(x) & P(x))))')
#-- saw no boy
VP1 = lp.parse(r'((\Q.\x1.Q(\x2.see(x1,x2)))(\P.-(exists x.(boy(x) & P(x)))))')
print VP1
print VP1.simplify()
#-- Cyril saw no boy
S6 = lp.parse(r'((\Q.\x1.Q(\x2.see(x1,x2)))(\P.-(exists x.(boy(x) & P(x)))))(cyril)')
print S6
print S6.simplify()



#Discourse Representation Theory (DRT)

#A discourse is a sequence of sentences. Often, the interpretation of a sentence in a discourse depends on the preceding sentences.

#For example, pronouns like `he', `she' and `it' can be anaphoric to (i.e., take their reference from) previous indefinite NPs.

#(18) Angus owns a dog. It bit Irene.

#The second sentence in (18) is interpreted as: The previously mentioned dog owned by Angus bit Irene.

#The first-order formula below does not capture this interpretation (why?):
print lp.parse(r'exists x.(dog(x) & own(angus,x)) & bite(x,irene)')

#The approach to quantification in first-order logic is limited to single sentences. But (18) seems to be a case in which the scope of a quantifier can extend over two sentences.
# -- the first-order formula below captures the correct interpretation of (18) (why exactly?)
print lp.parse(r'exists x.(dog(x) & own(angus,x) & bite(x,irene))')

#Discourse Representation Theory (DRT) was developed with the specific goal of providing a means for handling this and other semantic phenomena that seem to be characteristic of discourse.

# A discourse representation structure (DRS) presents the meaning of discourse in terms of a list of discourse referents (i.e., variables) and a list of conditions.
#-- the discourse referents are the things under discussion in the discourse; they correspond to the individual variables of first-order logic
#-- the DRS conditions constrain the value of those discourse referents; they correspond to (atomic) open formulas of first-order logic

#The first sentence in (18) is represented in DRT as follows.

dp = nltk.DrtParser()
drs1 = dp.parse('([x, y], [angus(x), dog(y), own(x, y)])')
print drs1
drs1.draw()

#Every DRS can be translated into a formula of first-order logic:
print drs1.fol()

#DRT expressions have a DRS-concatenation operator, represented as `+' (this is really just dynamic conjunction `;')

#-- the concatenation of two DRSs is a single DRS containing the merged discourse referents and the conditions from both arguments
#-- DRS-concatenation automatically alpha-converts bound variables to avoid name-clashes

drs2 = dp.parse('([z],[irene(z),bite(y,z)])')
print drs2
drs2.draw()

drs3 = dp.parse('([x, y], [angus(x), dog(y), own(x, y)]) + ([z],[irene(z),bite(y,z)])')
print drs3
drs3.draw()
drs3s = drs3.simplify()
print drs3s
drs3s.draw()

#-- note that the first-order translation of the simplified drs3s provides the intuitively correct truth conditions for discourse (18)
print drs3s.fol()

#-- let's see how alpha-conversion works
drs4 = dp.parse('([x, y], [angus(x), dog(y), own(x, y)]) + ([x],[irene(x),bite(y,x)])')
drs4s = drs4.simplify()
print drs4s
drs4s.draw()


#It is possible to embed one DRS within another. This is how universal quantification is handled.

drs5 = dp.parse('([], [(([x], [dog(x)]) -> ([y],[ankle(y), bite(x, y)]))])')
print drs5
drs5.draw()
print drs5.fol()


#We capture basic examples of donkey anaphora. Both sentences below are represented in the same way and the correct truth conditions (for the strong reading) are derived.

#(19) Every farmer who owns a donkey beats it.
#(20) If a farmer owns a donkey, he beats it.

drs6 = dp.parse('([], [(([x,y], [farmer(x),donkey(y),own(x,y)]) -> ([],[beat(x,y)]))])')
print drs6
drs6.draw()
print drs6.fol()


#DRT is designed to allow anaphoric pronouns to be interpreted by linking to existing discourse referents.

#When we represented (18) above, we automatically resolved the pronoun in the second sentence.

#But we want an explicit step of anaphora resolution because resolving anaphors is in general not as straightforward as it is in (18).

#DRT sets constraints on which discourse referents are "accessible" as possible antecedents, but does not intended to explain how a particular antecedent is chosen from the set of candidates.

# The module nltk.sem.drt_resolve_anaphora adopts a similarly conservative strategy:
#-- if a DRS contains a condition of the form PRO(x), the method resolve_anaphora() replaces this with a condition of the form x = [...], where [...] is a list of possible antecedents

drs7 = dp.parse('([x, y], [angus(x), dog(y), own(x, y)])')
drs8 = dp.parse('([u, z], [PRO(u), irene(z), bite(u, z)])')
drs9 = drs7 + drs8
print drs9.simplify()
print drs9.simplify().resolve_anaphora()

#Since the algorithm for anaphora resolution has been separated into its own module, this facilitates swapping in alternative procedures which try to make more intelligent guesses about the correct antecedent.