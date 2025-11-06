include Map.S with type key = int

val tup_list_encoding : 'a Json_encoding.encoding -> 'a t Json_encoding.encoding
val array_encoding : 'a Json_encoding.encoding -> 'a t Json_encoding.encoding
