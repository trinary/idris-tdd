

data PowerSource =  Petrol | Pedal | Electricity

data Vehicle  : PowerSource -> Type where
  Bicycle : Vehicle Pedal
  Unicycle : Vehicle Pedal
  Car : (fuel : Nat) -> Vehicle Petrol
  Bus : (fuel : Nat) -> Vehicle Petrol
  Motorcycle : (fuel : Nat) -> Vehicle Petrol
  Tram : (fuel : Nat) -> Vehicle Electricity

wheels : Vehicle power -> Nat
wheels Bicycle = 2
wheels Unicycle = 1
wheels (Car fuel) = 4
wheels (Bus fuel) = 4
wheels (Motorcycle fuel) = 2
wheels (Tram fuel) = 8

refuel : Vehicle power -> Vehicle power
refuel (Car fuel) = Car 100
refuel (Bus fuel) = Bus 200
refuel (Motorcycle fuel) = Motorcycle 75
refuel (Tram fuel) = Tram 400
refuel Bicycle = Bicycle
refuel Unicycle = Unicycle
