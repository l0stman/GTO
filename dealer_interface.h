#ifndef GTO_DEALER_INTERFACE_H_
#define GTO_DEALER_INTERFACE_H_

namespace GTO {

// Interface representing a dealer in a game.
class DealerInterface {
public:
        // Store in "hero_id" the state id of the first player and in
        // "vill_id" the state id of the second one.
        virtual void Deal(std::size_t& hero_id, std::size_t& vill_id) = 0;
};

} // namespace GTO

#endif  // !GTO_DEALER_INTERFACE_H_
