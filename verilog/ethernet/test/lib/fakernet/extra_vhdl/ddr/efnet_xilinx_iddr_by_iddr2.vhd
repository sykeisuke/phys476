-- Copyright (c) 2023, Haakan T. Johansson
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

library unisim;
use unisim.vcomponents.all;

entity efnet_hw_iddr is -- Note: not same name as file.
  port (
    c            : in  std_logic;
    d            : in  std_logic;
    q1           : out std_logic;
    q2           : out std_logic
    );
end efnet_hw_iddr;

architecture RTL of efnet_hw_iddr is
  signal not_c   : std_logic;
  signal q1_next : std_logic;
begin
  not_c <= not c;

  o: IDDR2
    generic map(
      ddr_alignment => "C0",
      srtype        => "ASYNC")
    port map (
      d  => d,
      c0 =>     c,
      c1 => not_c,
      ce => '1', R  => '0', S  => '0',
      q0 => q1_next,
      q1 => q2
      );

  process(c)
  begin
    if (rising_edge(c)) then
      -- Delay q1 by one cycle, such that q1 is value of rising edge
      -- of same cycle where q2 is value of falling edge.
      q1 <= q1_next;
    end if;
  end process;
end RTL;
