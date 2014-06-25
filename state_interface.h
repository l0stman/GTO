#ifndef GTO_STATE_INTERFACE_H_
#define GTO_STATE_INTERFACE_H_

#include <string>

namespace GTO {

// Interface representing the state of a player in a game.
class StateInterface {
public:
        // Return a string representing the name of the whole class.
        static std::string Name();

        // Return a string representing an individual state.
        virtual std::string ToString() const = 0;

        // Return the number of "concrete" states that were abstracted
        // by this one.  Should default to 1 in the general case.
        virtual unsigned short NumCombos() const = 0;
};

} // namespace GTO

#endif  // !GTO_STATE_INTERFACE_H_
