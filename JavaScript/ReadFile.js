//--------------------------------------------------------------------------------------------------
//
//  File:       ReadFile.js
//
//  Project:    M+M
//
//  Contains:   An example script that reads a series of values from a file.
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
//  Created:    2015-01-14
//
//--------------------------------------------------------------------------------------------------

var inStream = null;

var scriptDescription = 'A script that reads values from a file';

var scriptInlets = [ ];

var scriptOutlets = [ ];

function scriptStarting()
{
    var okSoFar = false;
    
    writeLineToStdout('script starting');
    if (1 < argv.length)
    {
        var path = argv[1];
        
        writeLineToStdout('path = ' + path);
        inStream = new Stream();
        inStream.open(path, "r");
        dumpObjectToStdout('inStream:', inStream);
        if (inStream.isOpen())
        {
            var inString;
            
            writeLineToStdout('inStream isOpen = true');
            writeLineToStdout('inStream atEof = ' + inStream.atEof());
            writeLineToStdout('inStream hasError = ' + inStream.hasError());
            for ( ; ! inStream.atEof(); )
            {
                inString = inStream.readLine();
                if (! inStream.atEof())
                {
                    writeLineToStdout('inStream read = "' + inString + '"');
                }
            }
        }
        okSoFar = true;
    }
    return okSoFar;
} // scriptStarting

function scriptStopping()
{
    writeLineToStdout('script stopping');
    inStream.close();
    inStream = null;
} // scriptStopping
