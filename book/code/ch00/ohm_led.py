"""Series resistor for an LED driven by a 3.3 V FPGA pin (Chapter 0).

Run:  python3 ohm_led.py
Ohm's law across the resistor:  R = (V_supply - V_LED) / I
Values are typical, not for a specific LED: check the datasheet of your LED.
"""
V_supply = 3.3      # V, the logic-high level of a 3.3 V output pin
V_led = 2.0         # V, typical forward voltage of a red LED (check datasheet)
I_target = 5e-3     # A, 5 mA is bright enough for an indicator LED

R = (V_supply - V_led) / I_target
print(f"R = ({V_supply} V - {V_led} V) / {I_target * 1e3:.0f} mA = {R:.0f} ohm")

# Pick the next standard (E12) value at or above R, so the current is not exceeded
E12 = [1.0, 1.2, 1.5, 1.8, 2.2, 2.7, 3.3, 3.9, 4.7, 5.6, 6.8, 8.2]
R_std = min(m * 10**k for k in range(0, 6) for m in E12 if m * 10**k >= R)
I = (V_supply - V_led) / R_std
print(f"Standard value: {R_std:.0f} ohm  ->  I = {I * 1e3:.2f} mA")
print(f"Power in the resistor: P = I^2 R = {I**2 * R_std * 1e3:.1f} mW")
print(f"Power in the LED:      P = V I   = {V_led * I * 1e3:.1f} mW")

print("\nWithout a resistor the current is limited only by the pin and the LED:")
print("too much current, and the LED or the FPGA pin can be damaged.")
