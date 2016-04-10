//--------------------------------------------------------------------------------------------------
//
//  File:       RecordIntegers.js
//
//  Project:    m+m
//
//  Contains:   An example script that writes a series of integer values to a file.
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
//  Created:    2015-01-13
//
//--------------------------------------------------------------------------------------------------

var outStream = null;

function doRecordIntegers(portNumber, incomingData)
{
    var aValue;

    if (Array.isArray(incomingData))
    {
        var mm = incomingData.length;

        for (var ii = 0; mm > ii; ++ii)
        {
            if (0 < ii)
            {
                outStream.write(' ');
            }
            aValue = Number(incomingData[ii]);
            if (! isNaN(aValue))
            {
                outStream.write(aValue);
            }
        }
        if (0 < mm)
        {
            outStream.writeLine('');
        }
    }
    else
    {
        aValue = Number(incomingData);
        if (! isNaN(aValue))
        {
            outStream.writeLine(aValue);
        }
    }
} // doRecordIntegers

var scriptDescription = 'A script that writes integer values to a file';

var scriptHelp = 'The first argument is the path to the output file';

var scriptInlets = [ { name: 'incoming', protocol: 'i+',
                        protocolDescription: 'A sequence of integer values',
                        handler: doRecordIntegers } ];

function scriptStarting()
{
    var okSoFar = false;

    writeLineToStdout('script starting');
    dumpObjectToStdout('argv:', argv);
    if (1 < argv.length)
    {
        var path = argv[1];

        writeLineToStdout('path = ' + path);
        outStream = new Stream();
        outStream.open(path, "w");
        dumpObjectToStdout('outStream:', outStream);
        if (outStream.isOpen())
        {
            writeLineToStdout('outStream isOpen = true');
            writeLineToStdout('outStream atEof = ' + outStream.atEof());
            writeLineToStdout('outStream hasError = ' + outStream.hasError());
        }
        okSoFar = true;
    }
    return okSoFar;
} // scriptStarting

function scriptStopping()
{
    writeLineToStdout('script stopping');
    outStream.close();
    outStream = null;
} // scriptStopping
