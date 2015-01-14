//--------------------------------------------------------------------------------------------------
//
//  File:       TruncateFloat.js
//
//  Project:    M+M
//
//  Contains:   An example script that drops the fractional part of floating-point numbers.
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
//  Created:    2015-01-13
//
//--------------------------------------------------------------------------------------------------

function doTruncateFloat(portNumber, incomingData)
{
    var aValue;
    var outValues;
    
    if (Array.isArray(incomingData))
    {
        outValues = [];
        for (var ii = 0, mm = incomingData.length; mm > ii; ++ii)
        {
            aValue = Number(incomingData[ii]);
            if (! isNaN(aValue))
            {
                outValues.push(Math.floor(aValue));
            }
        }
    }
    else
    {
        aValue = Number(incomingData);
        if (! isNaN(aValue))
        {
            outValues = Math.floor(aValue);
        }
    }
    sendToChannel(0, outValues);
} // doTruncateFloat

var scriptDescription = 'A script that truncates floating-point numbers';

var scriptInlets = [ { name: 'incoming', protocol: 'd*',
                        protocolDescription: 'A sequence of floating-point numbers',
                        handler: doTruncateFloat } ];

var scriptOutlets = [ { name: 'outgoing', protocol: 'i*',
                        protocolDescription: 'A sequence of integers' } ];
