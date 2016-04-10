//--------------------------------------------------------------------------------------------------
//
//  File:       SampleScript.js
//
//  Project:    m+m
//
//  Contains:   An example script that meets the requirements for a JavaScript file to be used with
//              the JavaScript filter service.
//
//  Written by: Norman Jaffe
//
//  Copyright:  (c) 2015 by H Plus Technologies Ltd. and Simon Fraser University.
//
//              All rights reserved. Redistribution and use in source and binary forms, with or
//              without modification, are permitted provided that the following conditions are met:
//                * Redistributions of source code must retain the above copyright notice, this list
//                  of conditions and the following disclaimer.
//                * Redistributions in binary form must reproduce the above copyright notice, this
//                  list of conditions and the following disclaimer in the documentation and / or
//                  other materials provided with the distribution.
//                * Neither the name of the copyright holders nor the names of its contributors may
//                  be used to endorse or promote products derived from this software without
//                  specific prior written permission.
//
//              THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY
//              EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
//              OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT
//              SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
//              INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
//              TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
//              BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
//              CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
//              ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
//              DAMAGE.
//
//  Created:    2015-01-06
//
//--------------------------------------------------------------------------------------------------

// Classes available to JavaScript code:
//
//   Stream: objects that connect to external text files
//     atEof() - returns true if the file is at end-of-file
//     clearError() - clears the end-of-file and error flags
//     close() - disconnects from the external file
//     hasError() - returns true if the error flag was set by a previous operation
//     isOpen() - returns true if there is an external file connected
//     open(fn, fm) - connects to an external file at path 'fn' with mode 'fm' [mode can be 'r',
//                    'w', et cetera]
//     readCharacter() - returns a string containing the next non-blank character from the external
//                       file
//     readLine() - returns a string containing the next line from the external file
//     readNumber() - returns the next numeric value from the external file, or zero. Note that EOF
//                    or error will be set if the number could not be read
//     readString() - returns a string from the external file; if the string starts with '"' or "'",
//                    it ends with the same character while, otherwise it starts with the next
//                    non-blank character and ends when a blank is read
//     rewind() - moves back to the beginning of the external file
//     write(...) - writes the arguments, as strings, to the external file
//     writeLine(...) - writes the arguments, as strings, to the external file and adds a newline
//
// Functions available to JavaScript code:
//
//   dumpObjectToStdout(t, x) - writes out the object 'x' to the standard output, including its
//                              properties, with a title of 't'
//
//   requestStop() - signal that the service should be stopped at the next opportunity
//
//   sendToChannel(n, x) - converts the value 'x' to YARP format and sends it to the channel
//                         numbered 'n', with zero being the first outlet channel
//
//   writeLineToStdout(x) - writes the string 'x' to the standard output.
//
// Global variables available to JavaScript code:
//
//   argv: a list of the arguments passed to the script
//
//   tag:  the (optional) tag argument for the JavaScript filter service
//
// Values that must be provided by the JavaScript code:
//
//   scriptDescription: a variable or a function that provides a string describing the script
//
// Values that may be provided by the JavaScript code:
//
//   scriptHelp:       a variable or a function that provides a string that can be presented to the
//                     user when requested by the '?' command; note that it should not end with a
//                     newline
//
//   scriptInlets:     a variable or a function that provides an array of inlet descriptions [name,
//                     protocol, protocolDescription, handler]; note that this is ignored if
//                     scriptThread() is defined
//
//   scriptInterval:   a variable or a function that provides the interval between executions of the
//                     scriptThread() function; note that this is ignored if scriptThread() is not
//                     defined, and it is executed only once, after all the other values have been
//                     processed
//
//   scriptOutlets:    a variable or a function that provides an array of outlet descriptions [name,
//                     protocol, protocolDescription]
//
//   scriptStarting(): a function that is called before any inlets are attached or threads started
//
//   scriptStopping(): a function that is called after all the inlets are detached and threads are
//                     stopped
//
//   scriptThread():   a function that is repeatedly called by the output thread of the service
//
// Order of reference / execution:
//
//   1)  Script is loaded; all global statements are executed, in order, and the global functions
//       are defined - functions and variables provided by the service are available, except for
//       'sendToChannel'
//   2)  'scriptDescription' is evaluated, if available
//   3)  'scriptHelp' is evaluated, if available
//   4)  'scriptThread' is retrieved, if available; this does not involve executing the function
//   5)  'scriptInlets' is evaluated, if available
//   6)  'scriptOutlets' is evaluated, if available
//   7)  'scriptStarting' is retrieved, if available; this does not involve executing the function
//   8)  'scriptStopping' is retrieved, if available; this does not involve executing the function
//   9)  'scriptInterval' is evaluated, if available and 'scriptThread' was retrieved
//  10)  ... configure the service ...; execute the 'scriptStarting' function, if available
//  11)  ... start service ...; inlets and outlets are created and attached
//  12)  'sendToChannel' can now be safely called
//  12a) If 'scriptThread' was retrieved, start a separate thread that executes the 'scriptThread'
//       function every 'scriptInterval' seconds
//  12b) If 'scriptThread' was not retrieved, associate the handler function of the 'scriptInlets'
//       value with its corresponding inlet, for all the inlets
//  13)  ... service running ...
//  14)  ... service stopping ...
//  14a) If 'scriptThread' was retrieved, the thread launched earlier is stopped
//  14b) If 'scriptThread' was not retrieved, the inlet stream handlers are deactivated
//  15)  Execute the 'scriptStopping' function, if available; 'sendToChannel' can be called
//  16)  ... inlet and outlet streams are destroyed, service is stopped ...

// Some test JavaScript...

writeLineToStdout('hello ' + 'world, it is ' + new Date());

var ww = 4.34, xx = 5;

function hereWeGo()
{
    writeLineToStdout('inside hereWeGo');
}

var yy = {};

var zz = { aa : 'first', bb : 'second', cc : 42};

function andAnotherFunction(aa)
{
    writeLineToStdout('argument is ' + aa);
    hereWeGo();
}

andAnotherFunction(42);
dumpObjectToStdout('argv:', argv);
writeLineToStdout('tag is "' + scriptTag + '"');
dumpObjectToStdout('global:', this);

// The real stuff:

function handleOurInput(portNumber, incomingData)
{
    writeLineToStdout('input on port ' + portNumber);
    // Convert the input to an integer, if it's a floating-point number; if it's an integer, pass it
    // through. If it's an array, process each element. If a value is non-numeric, report an error.
    sendToChannel(0, incomingData);
} // handleOurInput

// Specific named values required by the C++ code, such as 'scriptDescription' and 'scriptInlets',
// can be provided by either functions or 'global' variables.

//var scriptDescription = 'An example script';
function scriptDescription()
{
    return "An example script";
} // scriptDescription

var scriptHelp = 'The first argument to the script is the number of inlets to create';

// The following function will either generate one inlet, called 'incoming' or a set of inlets,
// called 'incoming#',
function scriptInlets()
{
    var inletCount;
    var inlets = [];

    if (1 < argv.length)
    {
        inletCount = parseInt(argv[1]);
        if (isNaN(inletCount))
        {
            inletCount = 1;
        }
    }
    if (1 < inletCount)
    {
        for (var ii = 0; inletCount > ii; ++ii)
        {
            inlets[ii] = { name: ('incoming' + (ii + 1)), protocol: '*',
                            protocolDescription: 'Anything',
                            handler: handleOurInput };
        }
    }
    else
    {
        inlets[0] = { name: 'incoming', protocol: '*',
                        protocolDescription: 'Anything',
                        handler: handleOurInput };
    }
    return inlets;
} // scriptInlets
//var scriptInlets = [];

var scriptOutlets = [ { name: 'outgoing', protocol: '*',
                        protocolDescription: 'Anything' } ];

// The 'scriptStarting' and 'scriptStopping' functions are optional; if 'scriptStarting' returns
// the boolean value true, it's OK to proceed. If, instead, it returns something else, the script
// has indicated that it shouldn't be run, and the return value gets displayed when the service is
// started - not when it's created or when the script is loaded. Note that 'scriptStarting' is
// executed before the handlers for the inlets are attached and 'scriptStopping' is executed after
// all the inlets are detached.
function scriptStarting()
{
    writeLineToStdout('script starting');
    return true;
} // scriptStarting

function scriptStopping()
{
    writeLineToStdout('script stopping');
} // scriptStopping
