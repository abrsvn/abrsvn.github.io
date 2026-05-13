# -*- coding: utf-8 -*-


"""
	Primitives that may be used in the LOT

"""

from Miscellaneous import *


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# For language / semantics
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

def presup_(a,b):
	if a: return b
	else: 
		if b: return "undefT" # distinguish these so that we can get presup out 
		else: return "undefF"

def is_undef(x):
	return (x is None) or (x =="undefT") or (x == "undefF") or (x == "undef")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Basic arithmetic
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
import math

def negative_(x): return -x

def plus_(x,y): return x+y
def times_(x,y): return x*y
def divide_(x,y): 
	if y > 0: return x/y
	else:     return float("inf")*x
def subtract_(x,y): return x-y

def sin_(x): 
	try:
		return math.sin(x)
	except: return float("nan")
def cos_(x): 
	try:
		return math.cos(x)
	except: return float("nan")
def tan_(x): 
	try:
		return math.tan(x)
	except: return float("nan")
	
def sqrt_(x): 
	try: return math.sqrt(x)
	except: return float("nan")
	
def pow_(x,y): 
	#print x,y
	try: return pow(x,y)
	except: return float("nan")

def exp_(x): 
	try: 
		r =math.exp(x)
		return x
	except: 
		return float("inf")*x
		
def log_(x): 
	if x > 0: return math.log(x)
	else: return -float("inf")

def log2_(x): 
	if x > 0: return math.log(x)/log(2.0)
	else: return -float("inf")
def pow2_(x): 
	return math.pow(2.0,x)
	
PI = math.pi
E = math.e

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Basic logic
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

def id_(A): return A # an identity function

def and_(A,B): return (A and B)
def or_(A,B): return (A or B)
def xor_(A,B): return (A and (not B)) or ((not A) and B)
def not_(A): return (not A)
def implies_(A,B): return (A or (not B))
def iff_(A,B): return ((A and B) or ((not A) and (not B)))

def if_(C,X,Y):
	if C: return X
	else: return Y
def ifU_(C,X):
	if C: return X
	else: return 'undef'
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Set-theoretic primitives
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

def union_(A,B): return A.union(B)
def intersection_(A,B): return A.intersection(B)
def setdifference_(A,B): return A.difference(B)
def select_(A): # choose an element, but don't remove it
	if len(A) > 0:
		x = A.pop()
		A.add(x)
		return set([x]) # but return a set
	else: return set() # empty set


def exhaustive_(A,B): return coextensive_(A,B)
def coextensive_(A,B): # are the two sets coextensive?
	return (A.issubset(B) and B.issubset(A))

def equal_(A,B): return (A == B)
def equal_word_(A,B): return (A == B)

def empty_(A): return (len(A)==0)
def nonempty_(A): return not empty_(A)
def cardinality1_(A): return (len(A)==1)
def cardinality2_(A): return (len(A)==2)
def cardinality3_(A): return (len(A)==3)
def cardinality4_(A): return (len(A)==4)
def cardinality5_(A): return (len(A)==5)
def cardinality_(A): return len(A)

# returns cardinalities of sets and otherwise numbers
def cardify(x):
	if isinstance(x, set): return len(x)
	else: return x;

def cardinalityeq_(A,B): return cardify(A) == cardify(B)
def cardinalitygt_(A,B): return cardify(A) > cardify(B)
def cardinalitylt_(A,B): return cardify(A) > cardify(B)

def subset_(A,B):
	return A.issubset(B)
	
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# counting list
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# the next word in the list -- we'll implement these as a hash table
word_list = ['one_', 'two_', 'three_', 'four_', 'five_', 'six_', 'seven_', 'eight_', 'nine_', 'ten_']
next_hash, prev_hash = [dict(), dict()]
for i in range(1, len(word_list)-1):
	next_hash[word_list[i]] = word_list[i+1]
	prev_hash[word_list[i]] = word_list[i-1]
next_hash['one_'] = 'two_'
next_hash['ten_'] = 'undef'
prev_hash['one_'] = 'undef'
prev_hash['ten_'] = 'nine_'
next_hash[None] = 'undef'
prev_hash[None] = 'undef'
next_hash['undef'] = 'undef'
prev_hash['undef'] = 'undef'
next_hash['X'] = 'X'
prev_hash['X'] = 'X'

word_to_number = dict() # map a word like 'four_' to its number, 4	
for i in range(len(word_list)):
	word_to_number[word_list[i]] = i+1

prev_hash[None] = None
def next_(w): return next_hash[w]
def prev_(w): return prev_hash[w]

# and define these
one_ = 'one_'
two_ = 'two_'
three_ = 'three_'
four_ = 'four_'
five_ = 'five_'
six_ = 'six_'
seven_ = 'seven_'
eight_ = 'eight_'
nine_ = 'nine_'
ten_ = 'ten_'
undef = 'undef'

	

