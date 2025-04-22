-- Copyright (c) 2021, Haakan T. Johansson
-- All rights reserved.

-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:
--     * Redistributions of source code must retain the above copyright
--       notice, this list of conditions and the following disclaimer.
--     * Redistributions in binary form must reproduce the above copyright
--       notice, this list of conditions and the following disclaimer in the
--       documentation and/or other materials provided with the distribution.
--     * Neither the name of the authors nor the
--       names of its contributors may be used to endorse or promote products
--       derived from this software without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
-- AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
-- ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
-- LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
-- CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
-- SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
-- INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
-- CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
-- ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
-- POSSIBILITY OF SUCH DAMAGE.

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- This dummy interface allows building when rataser is not available.
-- (A wrapper would have been just as messy.)

entity rataser_clock_send is
  generic(period_bits:   integer
          );
    port (clk:           in std_logic;

          tick_ns:       in std_logic_vector(63 downto 0);
          aux_sigs:      in std_logic_vector(4 downto 0);
          info_bit:      in std_logic;

          pulse_period_clks:  in std_logic_vector(period_bits-1 downto 0);
          duty_low_min_clks:  in std_logic_vector(period_bits-1 downto 0);
          duty_low_max_clks:  in std_logic_vector(period_bits-1 downto 0);
          eight_slot:    in std_logic;

          pulse_period_ns: in std_logic_vector(9 downto 0) := (others => '0');
          use_pulse_period_ns: in std_logic := '0';

          message_delay_ns: in std_logic_vector(21 downto 0) := (others =>'0');

          transmit:      out std_logic := '0';
          transmit_sync: out std_logic := '0';

          trail_short:   out std_logic := '0';
          trail_long:    out std_logic := '0'
          );
end;

architecture RTL of rataser_clock_send is

begin

  -- Some code to not be a black box.

  transmit <= '0';

end RTL;
