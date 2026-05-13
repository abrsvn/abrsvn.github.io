# -*- coding: utf-8 -*-

"""
	Miscellaneous functions for LOT-lib

	Steve Piantadosi - Sept 2011


"""
from scipy.maxentropy import logsumexp
from scipy.special import gammaln
import numpy as np
from random import random
import itertools
#from scipy.stats import norm
from math import exp, log, sqrt, pi, e
import functools # for memoize
import pickle
import time

from BasicPrimitives import * # For evaluating ..

import types # for checking if something is a function: isinstance(f, types.FunctionType)

Infinity = float("inf")
Null = []

def beta(a):
	"""
		Here a is a vector (of ints or floats) and this computes the Beta normalizing function,

	"""
	return np.sum(gammaln(np.array(a, dtype=float))) - gammaln(float(sum(a)))


def logsumexpadd(x,y):
	return logsumexp([x,y])

def q(x, quote='\"'):
	return quote+str(x)+quote

def display(x):
	print x

def lenr(x):
	return range(len(x))

def ifelse(x,y,z):
	if x: return y
	else: return z

def islist(x):
	return isinstance(x,list)

# add to a hash list
def hashplus(d, k, v=1):
	if not k in d: d[k] = v
	else: d[k] = d[k] + v

def flip(p):
	return random() < p

# for functional programming, print something and return it
def printr(x):
	print x
	return x


def generator_map(f, g):
	"""
		Map a function over a generator value, returning the mapped
	"""
	for x in g: yield f(x)

# joins by commas with none at the end. Correctly handles null lists
def commalist(listtext, sep1=', ', sep2=', '):
   return sep1.join(listtext[:-2]+['']) + sep2.join(listtext[-2:])

## TODO: Change this so that if N is large enough, you sort
# takes unnormalized probabilities and returns a list of the log probability and the object
# returnlist makes the return always a list (even if N=1); otherwise it is a list for N>1 only
# NOTE: This now can take probs as a function, which is then mapped!
def weighted_sample(objs, N=1, probs=None, log=False, return_probability=False, returnlist=False):
	# check how probabilities are specified
	# either as an argument, or attribute of objs (either probability or lp

	if len(objs) == 0: return None


	myprobs = None
	if probs is None:
		if hasattr(objs[0], 'probability'): # may be log or not
			myprobs = map(lambda x: x.probability, objs)
			log = False
		elif hasattr(objs[0], 'lp'): # MUST be logs
			myprobs = map(lambda x: x.lp, objs)
			log = True
		else:
			myprobs = [1.0] * len(objs) # sample uniform
	elif isinstance(probs, types.FunctionType): #NOTE: this does not work for class instance methods
		myprobs = map(probs, objs)
	else:
		myprobs = probs

	# make sure these are floats or things go badly
	myprobs = map(float, myprobs)

	# Now normalize and run
	Z = None
	if log: Z = logsumexp(myprobs)
	else: Z = sum(myprobs)

	out = []

	for n in range(N):
		r = random()
		for i in range(len(objs)):
			if log: r = r - exp(myprobs[i] - Z) # log domain
			else: r = r - (myprobs[i]/Z) # probability domain
			#print r, myprobs
			if r <= 0:
				if return_probability:
					lp = 0
					if log: lp = myprobs[i] - Z
					else:   lp = log(myprobs[i]) - log(Z)

					out.append( [objs[i],lp] )
					break
				else:
					out.append( objs[i] )
					break

	if N == 1 and (not returnlist): return out[0]  #don't give back a list if you just want one
	else:      return out


# The Y combinator
Y = lambda f: (lambda x: x(x)) (lambda y : f(lambda *args: y(y)(*args)) )
#example:
#fac = lambda f: lambda n: (1 if n<2 else n*(f(n-1)))
#Y(fac)(10)

# a fancy fixed point iterator that only goes MAX_RECURSION deep, else returning None
MAX_RECURSION = 25
def Y_bounded(f):
	return (lambda x, n: x(x, n)) (lambda y, n: f(lambda *args: y(y, n+1)(*args)) if n < MAX_RECURSION else None, 0)
#fac = lambda f: lambda n: (1 if n<2 else n*(f(n-1)))
#Y(fac)(10)

# here, e is an expression of the arguments.
# this adds lambdas and returns a function which is optionally recursive.
# if it is recursive, the "recurse" variable is what you use to call *this* function
def evaluate_expression(e, args=['x'], recurse="L_"):
	f = eval('lambda ' + recurse + ': lambda ' + commalist(args) + ' :' + str(e))
	return Y_bounded(f)
#example:
#g = evaluate_expression("x*L(x-1) if x > 1 else 1")
# g(12)
def lambdaone(*x): return 1
def lambdanull(*x): return []
def lambdaNone(*x): return x


# this takes a generator and by using a set (and thus a hash) it makes it unique, only returning each value once
# so this filters generators, making them unique
def make_generator_unique(gen):
	s = set()
	for gi in gen:
		if gi not in s:
			s.add(gi)
			yield gi



# this take sa dictionary d
# the keys of d must contain "lp", and d must contain counts
# this prints out a chi-squared test to make sure these are right
# NOTE: This doe snot do well if we have a fat tail, since we will necessarily sample some low probability events
from scipy.stats import chisquare
# importantly, throw out counts less than min_count -- else we get crummy
def test_expected_counts(d, display=True, sort=True, min_count=100):
	keys = d.keys() # maintain an order for the keys
	if sort:
		keys = sorted(keys, key=lambda x: d[x])
	lpZ = logsumexp([ k.lp for k in keys])
	cntZ = sum(d.values())
	if display:
		for k in keys:
			ocnt = float(d[k])/cntZ
			ecnt = exp(k.lp-lpZ)
			print d[k], "\t", ocnt, "\t", ecnt, "\t", ocnt/ecnt, "\t", k
	# now update these with their other probs
	keeper_keys = filter(lambda x: d[x] >= min_count, keys)
	#print len( keeper_keys), len(keys), map(lambda x: d[x], keys)
	lpZ = logsumexp([ k.lp for k in keeper_keys])
	cntZ = sum([d[k] for k in keeper_keys])

	# The chisquared test does not do well here iwth the low expected counts --
	print chisquare( [ d[k] for k in keeper_keys ], f_exp=array( [ cntZ * exp(k.lp - lpZ) for k in keeper_keys] ))  ##UGH expected *counts*, not probs


def normlogpdf(x, mu, sigma):
	"""
		The log pdf of a normal distribution
	"""
	#print x, mu
	return math.log(math.sqrt(2. * pi) * sigma) - ((x - mu) * (x - mu)) / (2.0 * sigma * sigma)

# the data here is a list of ordered pairs (x,y)
# f is a function taking x and mapping to y
def gaussian_likelihood(data, h, sd=1.0, args=['x']):
	#try:
	try:
		f = evaluate_expression(h.pystring(), args=args)
		sm = sum(map(lambda di: normlogpdf( f(di[0]), di[1], sd), data))
		return sm
	except: return float("-inf")


## AH INSTEAD USE itertools.product( [1,2,3], [5,6,7] ):
#def list_cross(*x):
	#"""
		#list_cross([1,2,3], [5,6,7]) => [1,5], [1,6], [1,7], [2,5], [2,6], ...
	#"""

	#if len(x) == 1:
		#for v in x[0]:
			#yield [v]
	#else:

		#first = x[0]
		#rest = x[1:]

		#for x in first:
			#for y in list_cross(*rest):
				#r = []
				#r.append(x)
				#r.extend(y)
				#yield r
#for g in list_cross( [1,2,3], [4,5,6], [7,8,9] ):
	#print g

## AAH INSTEAD USE itertools.product( [1,2,3], [5,6,7] )
#def generator_cross(*x):
	#"""
		#This takes a bunch of generators and crosses them, much like list_cross above
	#"""

	#if len(x) == 1:
		#for v in x[0]:
			#yield [ v ]
	#else:
		#first = x[0]
		#rest = x[1:]

		#xes = []

		#for y in generator_cross(*rest):

			## and get the next x
			#xes.append( first.next() )

			#for xi in xes:
				#r = []
				##print x,y,r
				#r.append(xi)
				#r.extend(y)
				#yield r

#for g in generator_cross( [1,2,3], [4,5,6], [7,8,9] ):
	#print g

#def fib(n):
    #if n==1 or n==0:
        #return 1
#return fib(n-2) + fib(n-1)
def pickle_save(x, f):
	out_file = open(f, 'wb')
	pickle.dump(x, out_file)
	out_file.close()
def pickle_load(f):
	in_file = open(f, 'r')
	r = pickle.load(in_file)
	in_file.close()
	return r










