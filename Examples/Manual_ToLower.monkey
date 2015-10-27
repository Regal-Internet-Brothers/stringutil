Strict

Public

' Imports:
Import regal.stringutil

' Functions:
Function Main:Int()
	' Local variable(s):
	
	' The string of characters we'll be manipulating.
	Local X:String = "Hello, world. This is a test."
	
	' Copy the contents of 'X' into a temporary array.
	Local Y:= X.ToChars()
	
	' Manually change each letter to lowercase:
	For Local I:= 0 Until Y.Length
		Y[I] = ToLower(Y[I])
	Next
	
	' Output to the console:
	Print("Normal: " + X)
	Print("Lowercase: " + String.FromChars(Y))
	
	' Return the default response.
	Return 0
End