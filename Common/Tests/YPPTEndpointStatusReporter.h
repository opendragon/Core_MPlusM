//
//  YPPTEndpointStatusReporter.h
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-02-28.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//

#if (! defined(YPPTENDPOINTSTATUSREPORTER_H_))
# define YPPTENDPOINTSTATUSREPORTER_H_ /* */

# include <yarp/os/Port.h>

namespace YarpPlusPlusTest
{
    /*! @brief The critical information for each volume.
     
     This class represents the information for a volume that is needed to perform the data delete operations. */
    class EndpointStatusReporter : public yarp::os::PortReport
    {
    public:
        
        /*! @brief The constructor. */
        EndpointStatusReporter(void);
        
        /*! @brief The destructor. */
        virtual ~EndpointStatusReporter(void);
        
        /*! @brief Write out the endpoint event / state information.
         @param info The event / state information from the endpoint. */
        virtual void report(const yarp::os::PortInfo & info);
        
    protected:
        
    private:
        
        typedef yarp::os::PortReport inherited;
        
    }; // EndpointStatusReporter
    
} // YarpPlusPlusTest

#endif // ! defined(YPPTENDPOINTSTATUSREPORTER_H_)
