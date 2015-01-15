//--------------------------------------------------------------------------------------------------
//
//  File:       RnadomBurst.js
//
//  Project:    M+M
//
//  Contains:   An example script that sends blocks of random numbers.
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
//  Created:    2015-01-15
//
//--------------------------------------------------------------------------------------------------

var burstSize;

var scriptDescription = 'A script that generates random blocks floating-point numbers';

var scriptHelp = 'The first argument is the burst period and the second argument is the burst size';

// Note that the following function processes both arguments, since it will be called at a specific
// point in the execution sequence.
function scriptInterval()
{
    var interval;
    var inlets = [];
    
    if (1 < argv.length)
    {
        interval = parseInt(argv[1]);
        if (isNaN(interval))
        {
            interval = 1;
        }
        if (2 < argv.length)
        {
            burstSize = parseInt(argv[2]);
            if (isNaN(burstSize))
            {
                burstSize = 1;
            }
        }
        else
        {
            burstSize = 1;
        }
    }
    else
    {
        burstSize = 1;
        interval = 1;
    }
    writeLineToStdout('burst interval is ' + interval + ' and burst size is ' + burstSize);
    return interval;
} // scriptInterval

var scriptOutlets = [ { name: 'output', protocol: 'd+',
                        protocolDescription: 'One or more numeric values' } ];

function scriptThread()
{
    var outList = [];
    
    for (var ii = 0; burstSize > ii; ++ii)
    {
        outList[ii] = (10000 * Math.random());
    }
    sendToChannel(0, outList);
} // scriptThread
