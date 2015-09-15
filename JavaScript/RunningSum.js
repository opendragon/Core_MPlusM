//--------------------------------------------------------------------------------------------------
//
//  File:       RunningSum.js
//
//  Project:    m+m
//
//  Contains:   An example script that performs a running sum.
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

var runningSum = 0;

function doCommand(aCommand, itsArgs)
{
    var aValue;
    
    switch (aCommand)
    {
        case 'add' :
            if (Array.isArray(itsArgs))
            {
                for (var ii = 0, mm = itsArgs.length; mm > ii; ++ii)
                {
                    aValue = Number(itsArgs[ii]);
                    if (! isNaN(aValue))
                    {
                        runningSum += Number(aValue);
                    }
                }
            }
            else
            {
                aValue = Number(itsArgs);
                if (! isNaN(aValue))
                {
                    runningSum += Number(aValue);
                }
            }
            break;
            
        case 'quit' :
            requestStop();
            break;
            
        case 'reset' :
            runningSum = 0;
            break;
            
        default :
            break;
            
    }
} // doCommand

function doRunningSum(portNumber, incomingData)
{
    if (Array.isArray(incomingData))
    {
        var cmd = String(incomingData.shift());
        
        doCommand(cmd, incomingData);
    }
    else
    {
        doCommand(String(incomingData), []);
    }
    sendToChannel(0, runningSum);
} // doRunningSum

var scriptDescription = 'A script that calculates running sums';

var scriptInlets = [ { name: 'incoming', protocol: 'sd*',
                        protocolDescription: 'A command and data',
                        handler: doRunningSum } ];

var scriptOutlets = [ { name: 'outgoing', protocol: 'd',
                        protocolDescription: 'The running sum' } ];
