//--------------------------------------------------------------------------------------------------
//
//  File:       SampleScript.js
//
//  Project:    M+M
//
//  Contains:   An example script that meets the requirements for a JavaScript file to be used with
//              the JavaScript input / output service.
//
//  Written by: Norman Jaffe
//
//  Copyright:  (c) 2015 by HPlus Technologies Ltd. and Simon Fraser University.
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

// The real stuff:

function scriptHandleInput(portNumber, incomingData)
{
    writeLineToStdout('input on port ' + portNumber);
    // Convert the input to an integer, if it's a floating-point number; if it's an integer, pass it
    // through. If it's an array, process each element. If a value is non-numeric, report an error.
    sendToChannel(0, incomingData);//portNumber.toString());
} // scriptHandleInput

// Specfic named values required by the C++ code, such as 'scriptDescription' and 'scriptInlets',
// can be provided by either functions or 'global' variables.

//var scriptDescription = 'An example script';
function scriptDescription()
{
    return "An example script";
} // scriptDescription

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
                            handler: scriptHandleInput };
        }
    }
    else
    {
        inlets[0] = { name: 'incoming', protocol: '*',
                        protocolDescription: 'Anything',
                        handler: scriptHandleInput };
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