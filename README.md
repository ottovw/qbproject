qb
====

### Introduction

QB is a small Scala library that allows the declaration of meta models, which are called schemas.

Its main use is where JSON data needs to be manipulated or transformed according to a description of your data.

Such a description looks like follows:
   
```
val todoSchema = qbClass(
  "title" -> qbString,
  "description" -> qbString,
  "dueDate" -> qbDateTime
)
```

Having a schema at hand, certain tasks like validation, transformation and generation of data are easy to accomplish. 

qb was designed with extensibility in mind and as such allows introducing new types or annotating existing types with rules or additional meta data. 

### Available components

QB currently is split up into three components. Additional components are likely to follow:

 - schema
	- provides the core data structures, the DSL and utility classes such as schema combinators to manipulate given schemas

	  *NOTE*: schema currently uses Play data structures such as JsValue and JsResult, and thus depends on Play, but this restriction will be removed in a future release
 - play
	- provides convenience types for using qb schema in conjunction with Play. Among others these are: 
		- a generic CRUD controller based on qb schema
		- a lightweight routing DSL
		- additional Actions like ValidatingActions 
 - csv: 
	- allows to transform CSV data into JSON 

QB is still a very young project, and as such, we appreciate any feedback, suggestions or contributions.

### Getting started

We will release a tutorial very soon. Stay tuned!


### Build

We use [Travis CI](http://travis-ci.org/) to build qb:
[![Build Status](https://travis-ci.org/qb-project/qb-code.svg?branch=master)](http://travis-ci.org/qb-project/qbproject)
