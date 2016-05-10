/*
 * The Lean Mean C++ Option Parser
 *
 * Copyright (C) 2012 Matthias S. Benkmann
 *
 * The "Software" in the following 2 paragraphs refers to this file containing
 * the code to The Lean Mean C++ Option Parser.
 * The "Software" does NOT refer to any other files which you
 * may have received alongside this file (e.g. as part of a larger project that
 * incorporates The Lean Mean C++ Option Parser).
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software, to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to permit
 * persons to whom the Software is furnished to do so, subject to the following
 * conditions:
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

#include "optionparser.hpp"

using namespace Option_;

#if defined(_MSC_VER)
# pragma intrinsic(_BitScanReverse)
int MSC_Builtin_CLZ::builtin_clz(const unsigned xx)
{
    unsigned long index;

    _BitScanReverse(&index, xx);
    return (32 - index); // int is always 32 bit on Windows, even for target x64
} // MSC_Builtin_CLZ::builtin_clz
#endif // defined(_MSC_VER)

Descriptor::Descriptor(const unsigned     index_,
                       const int          type_,
                       const char * const shortopt_,
                       const char * const longopt_,
                       const CheckArg     check_arg_,
                       const char *       help_) :
    index(index_), type(type_), shortopt(shortopt_), longopt(longopt_), check_arg(check_arg_),
    help(help_)
{
} // Descriptor::Descriptor

void
Option::append(Option * new_last)
{
    Option * pp = last();
    Option * ff = first();

    pp->next_ = new_last;
    new_last->prev_ = pp;
    new_last->next_ = tag(ff);
    ff->prev_ = tag(new_last);
} // Option::append

int
Option::count(void)
{
    int cc = (desc ? 1 : 0);

    for (Option * pp = first(); ! pp->isLast(); pp = pp->next_)
    {
        ++cc;
    }
    return cc;
} // Option::count

Option *
Option::first(void)
{
    Option * pp = this;

    for ( ; ! pp->isFirst(); pp = pp->prev_)
    {
    }
    return pp;
} // Option::first

void
Option::init(const Descriptor * desc_,
             const char *       name_,
             const char *       arg_)
{
    desc = desc_;
    name = name_;
    arg = arg_;
    prev_ = tag(this);
    next_ = tag(this);
    namelen = 0;
    if (name)
    {
        namelen = 1;
        if ('-' == *name)
        {
            for ( ; name[namelen] && ('=' != name[namelen]); )
            {
                ++namelen;
            }
        }
    }
} // Option::init

Option::Option(void) :
    desc(NULL), name(NULL), arg(NULL), namelen(0)
{
    prev_ = tag(this);
    next_ = tag(this);
} // Option::Option

Option::Option(const Descriptor * desc_,
               const char *       name_,
               const char *       arg_)
{
    init(desc_, name_, arg_);
} // Option::Option

Option::Option(const Option & orig)
{
    init(orig.desc, orig.name, orig.arg);
} // Option::Option

void
Option::operator =(const Option & orig)
{
    init(orig.desc, orig.name, orig.arg);
} // Option::operator =

Stats::Stats(void) :
    buffer_max(1), options_max(1) // 1 more than necessary as sentinel
{
} // Stats::Stats

Stats::Stats(const bool         gnu,
             const Descriptor * usage,
             const int          argc,
             const char * *     argv,
             const int          min_abbr_len,
             const bool         single_minus_longopt) :
    buffer_max(1), options_max(1) // 1 more than necessary as sentinel
{
    add(gnu, usage, argc, argv, min_abbr_len, single_minus_longopt);
} // Stats::Stats

Stats::Stats(const bool         gnu,
             const Descriptor * usage,
             const int          argc,
             char * *           argv,
             const int          min_abbr_len,
             const bool         single_minus_longopt) :
    buffer_max(1), options_max(1) // 1 more than necessary as sentinel
{
    add(gnu, usage, argc, const_cast<const char * *>(argv), min_abbr_len, single_minus_longopt);
} // Stats::Stats

Stats::Stats(const Descriptor * usage,
             const int          argc,
             const char * *     argv,
             const int          min_abbr_len,
             const bool         single_minus_longopt) :
    buffer_max(1), options_max(1) // 1 more than necessary as sentinel
{
    add(false, usage, argc, argv, min_abbr_len, single_minus_longopt);
} // Stats::Stats

Stats::Stats(const Descriptor * usage,
             const int          argc,
             char * *           argv,
             const int          min_abbr_len,
             const bool         single_minus_longopt) :
    buffer_max(1), options_max(1) // 1 more than necessary as sentinel
{
    add(false, usage, argc, const_cast<const char * *>(argv), min_abbr_len, single_minus_longopt);
} // Stats::Stats

void
Stats::add(const bool         gnu,
           const Descriptor * usage,
           const int          argc,
           const char * *     argv,
           const int          min_abbr_len,
           const bool         single_minus_longopt)
{
    // determine size of options array. This is the greatest index used in the usage + 1
    for (int ii = 0; usage[ii].shortopt; ++ii)
    {
        if ((usage[ii].index + 1) >= options_max)
        {
            options_max = (usage[ii].index + 1) + 1; // 1 more than necessary as sentinel
        }
    }
    CountOptionsAction action(&buffer_max);

    Parser::workhorse(gnu, usage, argc, argv, action, single_minus_longopt, false, min_abbr_len);
} // Stats::add

void
Stats::add(const bool         gnu,
           const Descriptor * usage,
           const int          argc,
           char * *           argv,
           const int          min_abbr_len,
           const bool         single_minus_longopt)
{
    add(gnu, usage, argc, const_cast<const char * *>(argv), min_abbr_len, single_minus_longopt);
} // Stats::add

void
Stats::add(const Descriptor * usage,
           const int          argc,
           const char * *     argv,
           const int          min_abbr_len,
           const bool         single_minus_longopt)
{
    add(false, usage, argc, argv, min_abbr_len, single_minus_longopt);
} // Stats::add

void
Stats::add(const Descriptor * usage,
           const int          argc,
           char * *           argv,
           const int          min_abbr_len,
           const bool         single_minus_longopt)
{
    add(false, usage, argc, const_cast<const char * *>(argv), min_abbr_len, single_minus_longopt);
} // Stats::add

Parser::Parser(void) :
    op_count(0), nonop_count(0), nonop_args(0), err(false)
{
} // Parser::Parser

Parser::Parser(const bool         gnu,
               const Descriptor * usage,
               const int          argc,
               const char * *     argv,
               Option *           options,
               Option *           buffer,
               const int          min_abbr_len,
               const bool         single_minus_longopt,
               const int          bufmax) :
    op_count(0), nonop_count(0), nonop_args(0), err(false)
{
    parse(gnu, usage, argc, argv, options, buffer, min_abbr_len, single_minus_longopt, bufmax);
} // Parser::Parser

Parser::Parser(const bool         gnu,
               const Descriptor * usage,
               const int          argc,
               char * *           argv,
               Option *           options,
               Option *           buffer,
               const int          min_abbr_len,
               const bool         single_minus_longopt,
               const int          bufmax) :
    op_count(0), nonop_count(0), nonop_args(0), err(false)
{
    parse(gnu, usage, argc, const_cast<const char * *>(argv), options, buffer, min_abbr_len,
          single_minus_longopt, bufmax);
} // Parser::Parser

Parser::Parser(const Descriptor * usage,
               const int          argc,
               const char * *     argv,
               Option *           options,
               Option *           buffer,
               const int          min_abbr_len,
               const bool         single_minus_longopt,
               const int          bufmax) :
    op_count(0), nonop_count(0), nonop_args(0), err(false)
{
    parse(false, usage, argc, argv, options, buffer, min_abbr_len, single_minus_longopt, bufmax);
} // Parser::Parser

Parser::Parser(const Descriptor * usage,
               const int          argc,
               char * *           argv,
               Option *           options,
               Option *           buffer,
               const int          min_abbr_len,
               const bool         single_minus_longopt,
               const int          bufmax) :
    op_count(0), nonop_count(0), nonop_args(0), err(false)
{
    parse(false, usage, argc, const_cast<const char * *>(argv), options, buffer, min_abbr_len,
          single_minus_longopt, bufmax);
} // Parser::Parser

void
Parser::parse(const bool         gnu,
              const Descriptor * usage,
              const int          argc,
              const char * *     argv,
              Option *           options,
              Option *           buffer,
              const int          min_abbr_len,
              const bool         single_minus_longopt,
              const int          bufmax)
{
    StoreOptionAction action(*this, options, buffer, bufmax);

    err = (! workhorse(gnu, usage, argc, argv, action, single_minus_longopt, true, min_abbr_len));
} // Parser::parse

void
Parser::parse(const bool         gnu,
              const Descriptor * usage,
              const int          argc,
              char * *           argv,
              Option *           options,
              Option *           buffer,
              const int          min_abbr_len,
              const bool         single_minus_longopt,
              const int          bufmax)
{
    parse(gnu, usage, argc, const_cast<const char * *>(argv), options, buffer, min_abbr_len,
          single_minus_longopt, bufmax);
} // Parser::parse

void
Parser::parse(const Descriptor * usage,
              const int          argc,
              const char * *     argv,
              Option *           options,
              Option *           buffer,
              const int          min_abbr_len,
              const bool         single_minus_longopt,
              const int          bufmax)
{
    parse(false, usage, argc, argv, options, buffer, min_abbr_len, single_minus_longopt, bufmax);
} // Parser::parse

void
Parser::parse(const Descriptor * usage,
              const int          argc,
              char * *           argv,
              Option *           options,
              Option *           buffer,
              const int          min_abbr_len,
              const bool         single_minus_longopt,
              const int          bufmax)
{
    parse(false, usage, argc, const_cast<const char * *>(argv), options, buffer, min_abbr_len,
          single_minus_longopt, bufmax);
} // Parser::parse

bool
Parser::workhorse(const bool         gnu,
                  const Descriptor * usage,
                  const int          numargsIn,
                  const char * *     args,
                  Action &           action,
                  const bool         single_minus_longopt,
                  const bool         print_errors,
                  const int          min_abbr_len)
{
    int numargs = numargsIn;
    int nonops = 0;

    // protect against NULL pointer
    if (! args)
    {
        numargs = 0;
    }
    for ( ; numargs && *args; )
    {
        const char * param = *args; // param can be --long-option, -srto or non-option argument

        // in POSIX mode the first non-option argument terminates the option list
        // a lone minus character is a non-option argument
        if (('-' != param[0]) || (! param[1]))
        {
            if (gnu)
            {
                ++nonops;
                ++args;
                if (numargs > 0)
                {
                    --numargs;
                }
                continue;
            }

            break;
        }

        // -- terminates the option list. The -- itself is skipped.
        if (('-' == param[1]) && (! param[2]))
        {
            shift(args, nonops);
            ++args;
            if (numargs > 0)
            {
                --numargs;
            }
            break;
        }

        bool         handle_short_options;
        const char * longopt_name;

        if ('-' == param[1]) // if --long-option
        {
            handle_short_options = false;
            longopt_name = param + 2;
        }
        else
        {
            handle_short_options = true;
            longopt_name = param + 1; //for testing a potential -long-option
        }
        bool try_single_minus_longopt = single_minus_longopt;
        bool have_more_args = ((numargs > 1) || (numargs < 0)); // is referencing argv[1] valid?

        do
        {
            // loop over short options in group, for long options the body is executed only once
            int          idx;
            const char * optarg;

            /******************** long option **********************/
            if ((! handle_short_options) || try_single_minus_longopt)
            {
                for (idx = 0; usage[idx].longopt && (! streq(usage[idx].longopt, longopt_name)); )
                {
                    ++idx;
                }
                if ((! usage[idx].longopt) && (min_abbr_len > 0))
                {
                    // if we should try to match abbreviated long options
                    int i1 = 0;

                    for ( ; usage[i1].longopt &&
                         (! streqabbr(usage[i1].longopt, longopt_name, min_abbr_len)); )
                    {
                        ++i1;
                    }
                    if (usage[i1].longopt)
                    {
                        // now test if the match is unambiguous by checking for another match
                        int i2 = i1 + 1;

                        for ( ; usage[i2].longopt &&
                             (! streqabbr(usage[i2].longopt, longopt_name, min_abbr_len)); )
                        {
                            ++i2;
                        }
                        if (! usage[i2].longopt)
                        {
                            // if there was no second match it's unambiguous, so accept i1 as idx
                            idx = i1;
                        }
                    }
                }
                // if we found something, disable handle_short_options (only relevant if
                // single_minus_longopt)
                if (usage[idx].longopt)
                {
                    handle_short_options = false;
                }
                try_single_minus_longopt = false; // prevent looking for longopt in the middle of
                                                  // shortopt group
                for (optarg = longopt_name; *optarg && ('=' != *optarg); )
                {
                    ++optarg;
                }
                if ('=' == *optarg)
                {
                    // attached argument
                    ++optarg;
                }
                else
                {
                    // possibly detached argument
                    optarg = (have_more_args ? args[1] : NULL);
                }
            }
            /************************ short option ***********************************/
            if (handle_short_options)
            {
                if (! *++param) // point at the 1st/next option character
                {
                    break; // end of short option group
                }

                for (idx = 0; usage[idx].shortopt && (! instr(*param, usage[idx].shortopt)); )
                {
                    ++idx;
                }
                if (param[1])
                {
                    // if the potential argument is attached
                    optarg = param + 1;
                }
                else
                {
                    // if the potential argument is separate
                    optarg = (have_more_args ? args[1] : NULL);
                }
            }
            const Descriptor * descriptor = &usage[idx];

            if (! descriptor->shortopt)
            {
                /**************  unknown option ********************/
                // look for dummy entry (shortopt == "" and longopt == "") to use as Descriptor for
                // unknown options
                for (idx = 0;
                     usage[idx].shortopt && (usage[idx].shortopt[0] || usage[idx].longopt[0]); )
                {
                    ++idx;
                }
                descriptor = (usage[idx].shortopt ? &usage[idx] : NULL);
            }
            if (descriptor)
            {
                Option option(descriptor, param, optarg);

                switch (descriptor->check_arg(option, print_errors))
                {
                    case ARG_ILLEGAL:
                        return false; // fatal

                    case ARG_OK:
                        // skip one element of the argument vector, if it's a separated argument
                        if (optarg && have_more_args && (optarg == args[1]))
                        {
                            shift(args, nonops);
                            if (numargs > 0)
                            {
                                --numargs;
                            }
                            ++args;
                        }
                        // No further short options are possible after an argument
                        handle_short_options = false;
                        break;

                    case ARG_IGNORE:
                    case ARG_NONE:
                        option.arg = 0;
                        break;

                }
                if (! action.perform(option))
                {
                    return false;
                }

            }
        }
        while (handle_short_options);
        shift(args, nonops);
        ++args;
        if (numargs > 0)
        {
            --numargs;
        }
    }
    if ((numargs > 0) && (! *args))
    {
        // It's a bug in the caller if numargs is greater than the actual number
        numargs = 0; // of arguments, but as a service to the user we fix this if we spot it.
    }
    if (numargs < 0) // if we don't know the number of remaining non-option arguments
    {
        // we need to count them
        numargs = 0;
        for ( ; args[numargs]; )
        {
            ++numargs;
        }
    }
    return action.finished(numargs + nonops, args - nonops);
} // Parser::workhorse

bool
Parser::streq(const char * st1,
              const char * st2)
{
    for ( ; *st1; )
    {
        if (*st1++ != *st2++)
        {
            return false;
        }
    }
    return ((! *st2) || ('=' == *st2));
} // Parser::streq

bool
Parser::streqabbr(const char * st1,
                  const char * st2,
                  const size_t min)
{
    const char * st1start = st1;

    for ( ; *st1 && (*st1 == *st2); )
    {
        ++st1;
        ++st2;
    }
    return (((! *st1) || ((min > 0) && ((st1 - st1start) >= static_cast<int>(min)))) &&
            ((! *st2) || ('=' == *st2)));
} // Parser::streqabbr

bool
Parser::instr(const char   ch,
              const char * st)
{
    for ( ; *st && (ch != *st); )
    {
        ++st;
    }
    return (ch == *st);
} // Parser::instr

void
Parser::shift(const char * * args,
              const int      count)
{
    for (int ii = 0; ii > (- count); --ii)
    {
        const char * temp = args[ii];

        args[ii] = args[ii - 1];
        args[ii - 1] = temp;
    }
} // Parser::shift

bool
Parser::Action::finished(const int      numargs,
                         const char * * args)
{
    (void) numargs;
    (void) args;
    return true;
} // Parser::Action::finished

bool
Parser::Action::perform(Option &)
{
    return true;
} // Parser::Action::perform

Stats::CountOptionsAction::CountOptionsAction(unsigned * buffer_max_) :
    buffer_max(buffer_max_)
{
} // Stats::CountOptionsAction::CountOptionsAction

bool
Stats::CountOptionsAction::perform(Option &)
{
    if (0x7fffffff == *buffer_max)
    {
        return false; // overflow protection: don't accept number of options that doesn't fit signed
                      // int
    }
    ++*buffer_max;
    return true;
} // Stats::CountOptionsAction::perform

Parser::StoreOptionAction::StoreOptionAction(Parser &  parser_,
                                             Option *  options_,
                                             Option *  buffer_,
                                             const int bufmax_) :
    parser(parser_), options(options_), buffer(buffer_), bufmax(bufmax_)
{
    // find first empty slot in buffer (if any)
    int bufidx = 0;

    for ( ; ((bufmax < 0) || (bufidx < bufmax)) && buffer[bufidx]; )
    {
        ++bufidx;
    }
    // set parser's optionCount
    parser.op_count = bufidx;
} // Parser::StoreOptionAction::StoreOptionAction

bool
Parser::StoreOptionAction::finished(const int      numargs,
                                    const char * * args)
{
    // only overwrite non-option argument list if there's at least 1
    // new non-option argument. Otherwise we keep the old list. This
    // makes it easy to use default non-option arguments.
    if (numargs > 0)
    {
        parser.nonop_count = numargs;
        parser.nonop_args = args;
    }
    return true;
} // Parser::StoreOptionAction::finished

bool
Parser::StoreOptionAction::perform(Option & option)
{
    if ((bufmax < 0) || (parser.op_count < bufmax))
    {
        if (0x7fffffff == parser.op_count)
        {
            return false; // overflow protection: don't accept number of options that doesn't fit
                          // signed int
        }

        buffer[parser.op_count] = option;
        int idx = buffer[parser.op_count].desc->index;

        if (options[idx])
        {
            options[idx].append(buffer[parser.op_count]);
        }
        else
        {
            options[idx] = buffer[parser.op_count];
        }
        ++parser.op_count;
    }
    return true; // NOTE: an option that is discarded because of a full buffer is not fatal
} // Parser::StoreOptionAction::perform

void
PrintUsageImplementation::IStringWriter::operator ()(const char *,
                                                     const int)
{
} // PrintUsageImplementation::IStringWriter::operator ()

void
PrintUsageImplementation::indent(IStringWriter & write,
                                 int &           xx,
                                 const int       want_x)
{
    int indent = want_x - xx;

    if (indent < 0)
    {
        write("\n", 1);
        indent = want_x;
    }
    if (indent > 0)
    {
        char space = ' ';

        for (int ii = 0; ii < indent; ++ii)
        {
            write(&space, 1);
        }
        xx = want_x;
    }
} // PrintUsageImplementation::indent

bool
PrintUsageImplementation::isWideChar(const unsigned ch)
{
    if (0x303F == ch)
    {
        return false;
    }

    return (((0x1100 <= ch) && (ch <= 0x115F)) || ((0x2329 <= ch) && (ch <= 0x232A)) ||
            ((0x2E80 <= ch) && (ch <= 0xA4C6)) || ((0xA960 <= ch) && (ch <= 0xA97C)) ||
            ((0xAC00 <= ch) && (ch <= 0xD7FB)) || ((0xF900 <= ch) && (ch <= 0xFAFF)) ||
            ((0xFE10 <= ch) && (ch <= 0xFE6B)) || ((0xFF01 <= ch) && (ch <= 0xFF60)) ||
            ((0xFFE0 <= ch) && (ch <= 0xFFE6)) || ((0x1B000 <= ch)));
} // PrintUsageImplementation::isWideChar

void
PrintUsageImplementation::LinePartIterator::update_length(void)
{
    screenlen = 0;
    for (len = 0; ptr[len] && ('\v' != ptr[len]) && ('\t' != ptr[len]) && ('\n' != ptr[len]); ++len)
    {
        ++screenlen;
        unsigned ch = static_cast<unsigned char>(ptr[len]);

        if (ch > 0xC1)
        {
            // everything <= 0xC1 (yes, even 0xC1 itself) is not a valid UTF-8 start byte
            // int __builtin_clz (unsigned int x)
            // Returns the number of leading 0-bits in x, starting at the most significant bit
            unsigned mask = (static_cast<unsigned>(-1) >> __builtin_clz(ch ^ 0xff));

            ch = ch & mask; // mask out length bits, we don't verify their correctness
            for ( ; (static_cast<unsigned char>(ptr[len + 1]) ^ 0x80) <= 0x3F; )
            {
                // while next byte is continuation byte
                ch = ((ch << 6) ^ static_cast<unsigned char>(ptr[len + 1]) ^ 0x80);
                // add continuation to char code
                ++len;
            }
            // ch is the decoded unicode code point
            if ((0x1100 <= ch) && isWideChar(ch))
            {
                // the test for 0x1100 is here to avoid the function call in the Latin case
                ++screenlen;
            }
        }
    }
} // PrintUsageImplementation::LinePartIterator::update_length

PrintUsageImplementation::LinePartIterator::LinePartIterator(const Descriptor * usage) :
    tablestart(usage), rowdesc(0), rowstart(0), ptr(0), col(-1), len(0), max_line_in_block(0),
    line_in_block(0), target_line_in_block(0), hit_target_line(true)
{
} // PrintUsageImplementation::LinePartIterator::LinePartIterator

bool
PrintUsageImplementation::LinePartIterator::nextTable(void)
{
    // If this is NOT the first time nextTable() is called after the constructor,
    // then skip to the next table break (i.e. a Descriptor with help == 0)
    if (rowdesc)
    {
        for ( ; tablestart->help && tablestart->shortopt; )
        {
            ++tablestart;
        }
    }
    // Find the next table after the break (if any)
    for ( ; (! tablestart->help) && tablestart->shortopt; )
    {
        ++tablestart;
    }
    restartTable();
    return (0 != rowstart);
} // PrintUsageImplementation::LinePartIterator::nextTable

void
PrintUsageImplementation::LinePartIterator::restartTable(void)
{
    rowdesc = tablestart;
    rowstart = tablestart->help;
    ptr = NULL;
} // PrintUsageImplementation::LinePartIterator::restartTable

bool
PrintUsageImplementation::LinePartIterator::nextRow(void)
{
    if (! ptr)
    {
        restartRow();
        return rowstart != 0;
    }

    for ( ; *ptr && ('\n' != *ptr); )
    {
        ++ptr;
    }
    if (! *ptr)
    {
        if (! (rowdesc + 1)->help)
        {
            // table break
            return false;
        }

        ++rowdesc;
        rowstart = rowdesc->help;
    }
    else // if (*ptr == '\n')
    {
        rowstart = ptr + 1;
    }
    restartRow();
    return true;
} // PrintUsageImplementation::LinePartIterator::nextRow

void
PrintUsageImplementation::LinePartIterator::restartRow(void)
{
    ptr = rowstart;
    col = -1;
    len = 0;
    screenlen = 0;
    max_line_in_block = 0;
    line_in_block = 0;
    target_line_in_block = 0;
    hit_target_line = true;
} // PrintUsageImplementation::LinePartIterator::restartRow

bool
PrintUsageImplementation::LinePartIterator::next(void)
{
    if (! ptr)
    {
        return false;
    }

    if (col == -1)
    {
        col = 0;
        update_length();
        return true;
    }

    ptr += len;
    for ( ; ; )
    {
        switch (*ptr)
        {
            case '\v':
                upmax(max_line_in_block, ++line_in_block);
                ++ptr;
                break;

            case '\t':
                if (! hit_target_line)
                {
                    // if previous column did not have the targetline
                    // then "insert" a 0-length part
                    update_length();
                    hit_target_line = true;
                    return true;
                }

                hit_target_line = false;
                line_in_block = 0;
                ++col;
                ++ptr;
                break;

            case 0:
            case '\n':
                if (! hit_target_line)
                {
                    // if previous column did not have the targetline
                    // then "insert" a 0-length part
                    update_length();
                    hit_target_line = true;
                    return true;
                }

                if (++target_line_in_block > max_line_in_block)
                {
                    update_length();
                    return false;
                }

                hit_target_line = false;
                line_in_block = 0;
                col = 0;
                ptr = rowstart;
                continue;

            default:
                ++ptr;
                continue;

        } // switch

        if (line_in_block == target_line_in_block)
        {
            update_length();
            hit_target_line = true;
            return true;
        }
    }
} // PrintUsageImplementation::LinePartIterator::next

void
PrintUsageImplementation::LineWrapper::buf_store(const char * data,
                                                 const int    len)
{
    lenbuf[head] = len;
    datbuf[head] = data;
    head = ((head + 1) & bufmask);
} // PrintUsageImplementation::LineWrapper::buf_store

void
PrintUsageImplementation::LineWrapper::output(IStringWriter & write,
                                              const char *    data,
                                              const int       len)
{
    if (buf_full())
    {
        write_one_line(write);
    }
    buf_store(data, len);
} // PrintUsageImplementation::LineWrapper::output

void
PrintUsageImplementation::LineWrapper::write_one_line(IStringWriter & write)
{
    if (wrote_something) // if we already wrote something, we need to start a new line
    {
        write("\n", 1);
        int _x = 0;

        indent(write, _x, x);
    }
    if (! buf_empty())
    {
        buf_next();
        write(datbuf[tail], lenbuf[tail]);
    }
    wrote_something = true;
} // PrintUsageImplementation::LineWrapper::write_one_line

void
PrintUsageImplementation::LineWrapper::flush(IStringWriter & write)
{
    if (! buf_empty())
    {
        int _x = 0;

        indent(write, _x, x);
        wrote_something = false;
        for ( ; ! buf_empty(); )
        {
            write_one_line(write);
        }
        write("\n", 1);
    }
} // PrintUsageImplementation::LineWrapper::flush

void
PrintUsageImplementation::LineWrapper::process(IStringWriter & write,
                                               const char *    data,
                                               int             len)
{
    wrote_something = false;
    for ( ; len > 0 ; )
    {
        if (len <= width)
        {
            // quick test that works because utf8width <= len (all wide chars have at least 2 bytes)
            output(write, data, len);
            len = 0;
        }
        else // if (len > width)  it's possible (but not guaranteed) that utf8len > width
        {
            int maxi = 0;

            for (int utf8width = 0; (maxi < len) && (utf8width < width); )
            {
                int      charbytes = 1;
                unsigned ch = static_cast<unsigned char>(data[maxi]);

                if (ch > 0xC1)
                {
                    // everything <= 0xC1 (yes, even 0xC1 itself) is not a valid UTF-8 start byte
                    // int __builtin_clz (unsigned int x)
                    // Returns the number of leading 0-bits in x, starting at the most significant bit
                    unsigned mask = (static_cast<unsigned>(-1) >> __builtin_clz(ch ^ 0xff));

                    ch = ch & mask; // mask out length bits, we don't verify their correctness
                    for ( ; (maxi + charbytes < len) &&
                         ((static_cast<unsigned char>(data[maxi + charbytes]) ^ 0x80) <= 0x3F); )
                    {
                        // while next byte is continuation byte
                        ch = ((ch << 6) ^ static_cast<unsigned char>(data[maxi + charbytes]) ^
                              0x80);
                        // add continuation to char code
                        ++charbytes;
                    }
                    // ch is the decoded unicode code point
                    if ((0x1100 <= ch) && isWideChar(ch))
                    {
                        // the test for 0x1100 is here to avoid the function call in the Latin case
                        if ((utf8width + 2) > width)
                        {
                            break;
                        }

                        ++utf8width;
                    }
                }
                ++utf8width;
                maxi += charbytes;
            }
            // data[maxi-1] is the last byte of the UTF-8 sequence of the last character that fits
            // onto the 1st line. If maxi == len, all characters fit on the line.
            if (maxi == len)
            {
                output(write, data, len);
                len = 0;
            }
            else // if (maxi < len)  at least 1 character (data[maxi] that is) doesn't fit on the line
            {
                int ii;

                for (ii = maxi; ii >= 0; --ii)
                {
                    if (' ' == data[ii])
                    {
                        break;
                    }

                }
                if (ii >= 0)
                {
                    output(write, data, ii);
                    data += ii + 1;
                    len -= ii + 1;
                }
                else
                {
                    // did not find a space to split at => split before data[maxi]
                    // data[maxi] is always the beginning of a character, never a continuation byte
                    output(write, data, maxi);
                    data += maxi;
                    len -= maxi;
                }
            }
        }
    }
    if (! wrote_something)
    {
        // if we didn't already write something to make space in the buffer
        write_one_line(write); // write at most one line of actual output
    }
} // PrintUsageImplementation::LineWrapper::process

PrintUsageImplementation::LineWrapper::LineWrapper(const int x1,
                                                   const int x2) :
    x(x1), width(x2 - x1), head(0), tail(bufmask)
{
    if (width < 2)
    {
        // because of wide characters we need at least width 2 or the code breaks
        width = 2;
    }
} // PrintUsageImplementation::LineWrapper::LineWrapper

void
PrintUsageImplementation::printUsage(IStringWriter &    write,
                                     const Descriptor * usage,
                                     const int          widthIn,
                                     const int          last_column_min_percent,
                                     const int          last_column_own_line_max_percent)
{
    int width = widthIn;

    if (width < 1)
    {
        // protect against nonsense values
        width = 80;
    }
    if (width > 10000)
    {
        // protect against overflow in the following computation
        width = 10000;
    }
    int last_column_min_width = (((width * last_column_min_percent) + 50) / 100);
    int last_column_own_line_max_width = (((width * last_column_own_line_max_percent) + 50) / 100);

    if (0 == last_column_own_line_max_width)
    {
        last_column_own_line_max_width = 1;
    }
    LinePartIterator part(usage);

    for ( ; part.nextTable(); )
    {
        /***************** Determine column widths *******************************/

        const int maxcolumns = 8; // 8 columns are enough for everyone
        int       col_width[maxcolumns];
        int       lastcolumn;
        int       leftwidth;
        int       overlong_column_threshold = 10000;

        do
        {
            lastcolumn = 0;
            for (int ii = 0; ii < maxcolumns; ++ii)
            {
                col_width[ii] = 0;
            }
            part.restartTable();
            for ( ; part.nextRow(); )
            {
                for ( ; part.next(); )
                {
                    if (part.column() < maxcolumns)
                    {
                        upmax(lastcolumn, part.column());
                        if (part.screenLength() < overlong_column_threshold)
                        {
                            // We don't let rows that don't use table separators (\t or \v) influence
                            // the width of column 0. This allows the user to interject section headers
                            // or explanatory paragraphs that do not participate in the table layout.
                            if ((part.column() > 0) || (part.line() > 0) ||
                                ('\t' == part.data()[part.length()]) ||
                                ('\v' == part.data()[part.length()]))
                            {
                                upmax(col_width[part.column()], part.screenLength());
                            }
                        }
                    }
                }
            }

            /*
             * If the last column doesn't fit on the same
             * line as the other columns, we can fix that by starting it on its own line.
             * However we can't do this for any of the columns 0..lastcolumn-1.
             * If their sum exceeds the maximum width we try to fix this by iteratively
             * ignoring the widest line parts in the width determination until
             * we arrive at a series of column widths that fit into one line.
             * The result is a layout where everything is nicely formatted
             * except for a few overlong fragments.
             * */

            leftwidth = 0;
            overlong_column_threshold = 0;
            for (int ii = 0; ii < lastcolumn; ++ii)
            {
                leftwidth += col_width[ii];
                upmax(overlong_column_threshold, col_width[ii]);
            }

        }
        while (leftwidth > width);

        /**************** Determine tab stops and last column handling **********************/
        int tabstop[maxcolumns];

        tabstop[0] = 0;
        for (int ii = 1; ii < maxcolumns; ++ii)
        {
            tabstop[ii] = tabstop[ii - 1] + col_width[ii - 1];
        }
        int  rightwidth = width - tabstop[lastcolumn];
        bool print_last_column_on_own_line = false;

        if ((rightwidth < last_column_min_width) && (rightwidth < col_width[lastcolumn]))
        {
            print_last_column_on_own_line = true;
            rightwidth = last_column_own_line_max_width;
        }

        // If lastcolumn == 0 we must disable print_last_column_on_own_line because
        // otherwise 2 copies of the last (and only) column would be output.
        // Actually this is just defensive programming. It is currently not
        // possible that lastcolumn==0 and print_last_column_on_own_line==true
        // at the same time, because lastcolumn==0 => tabstop[lastcolumn] == 0 =>
        // rightwidth==width => rightwidth>=last_column_min_width  (unless someone passes
        // a bullshit value >100 for last_column_min_percent) => the above if condition
        // is false => print_last_column_on_own_line==false
        if (0 == lastcolumn)
        {
            print_last_column_on_own_line = false;
        }
        LineWrapper lastColumnLineWrapper(width - rightwidth, width);
        LineWrapper interjectionLineWrapper(0, width);

        part.restartTable();
        /***************** Print out all rows of the table *************************************/
        for ( ; part.nextRow(); )
        {
            int xx = -1;

            for ( ; part.next(); )
            {
                if (part.column() > lastcolumn)
                {
                    continue; // drop excess columns (can happen if lastcolumn == maxcolumns-1)
                }

                if (part.column() == 0)
                {
                    if (xx >= 0)
                    {
                        write("\n", 1);
                    }
                    xx = 0;
                }
                indent(write, xx, tabstop[part.column()]);
                if ((part.column() < lastcolumn) && ((part.column() > 0) || (part.line() > 0) ||
                                                     ('\t' == part.data()[part.length()]) ||
                                                     ('\v' == part.data()[part.length()])))
                {
                    write(part.data(), part.length());
                    xx += part.screenLength();
                }
                else
                {
                    // either part.column() == lastcolumn or we are in the special case of
                    // an interjection that doesn't contain \v or \t
                    // NOTE: This code block is not necessarily executed for
                    // each line, because some rows may have fewer columns.

                    LineWrapper & lineWrapper = ((part.column() == 0) ? interjectionLineWrapper :
                                                 lastColumnLineWrapper);

                    if (! print_last_column_on_own_line)
                    {
                        lineWrapper.process(write, part.data(), part.length());
                    }
                }
            }
            if (print_last_column_on_own_line)
            {
                part.restartRow();
                for ( ; part.next(); )
                {
                    if (part.column() == lastcolumn)
                    {
                        write("\n", 1);
                        int _x = 0;

                        indent(write, _x, width - rightwidth);
                        lastColumnLineWrapper.process(write, part.data(), part.length());
                    }
                }
            }
            write("\n", 1);
            lastColumnLineWrapper.flush(write);
            interjectionLineWrapper.flush(write);
        }
    }
} // PrintUsageImplementation::printUsage
