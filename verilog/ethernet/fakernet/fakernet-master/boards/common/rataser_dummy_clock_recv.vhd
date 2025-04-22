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

entity rataser_clock_recv is
  generic(period_bits: integer;

          num_last_edges: integer;

          pipeline_sym24_diff_in   : boolean := true;
          pipeline_sym24_diff_1    : boolean := true;
          pipeline_sym24_diff_2    : boolean := true;
          pipeline_sym24_diff_out  : boolean := true;
          pipeline_sym24_match_out : boolean := true;
          pipeline_sym24_pick      : boolean := true;
          pipeline_sym24_out       : boolean := true;

          pipeline_hamming_parity  : boolean := true;
          pipeline_hamming_dual    : boolean := true;

          max_skew_4phase_sample   : string := "0.5 ns");
  port (clk:           in std_logic;

        clk90:         in std_logic;

        receive:       in std_logic;
        eight_slot:    in std_logic;

        expect_edge:   in std_logic_vector(1 downto 0);
        last_edges:    out std_logic_vector(num_last_edges*2-1 downto 0);
        use_auto_edge: in std_logic;
        auto_edge:     out std_logic_vector(1 downto 0);

        receive_delay_ns: in std_logic_vector(15 downto 0) := (others => '0');

        tick_ns_lo:    out std_logic_vector(31 downto  0);
        tick_ns_hi:    out std_logic_vector(63 downto 32);
        aux_sigs:      out std_logic_vector(4 downto 0);
        info_bit:      out std_logic;
        msg_strobe:    out std_logic;

        sync_status:   out std_logic_vector(2 downto 0);
        sync_lost:     out std_logic_vector(2 downto 0);
        bad_signals:   out std_logic_vector(4 downto 0);
        clear_status:  in std_logic;

        pulse_period_clks: in std_logic_vector(period_bits-1 downto 0);
        clk_period_ns: in std_logic_vector(9 downto 0);

        sender_period_ns: out std_logic_vector(9 downto 0);
        track_incr_per_clock: out std_logic_vector(15 downto 0);

        pulse_length_min: out std_logic_vector(9 downto 0);
        pulse_length_max: out std_logic_vector(9 downto 0);
        pulse_length_diff6: out std_logic_vector(10 downto 0)

        -- sym8_debug:    out rataser_sym8word1_debug
        );
end;

architecture RTL of rataser_clock_recv is

begin

  -- Some code to not be a black box.

  tick_ns_lo <= (others => '0');
  tick_ns_hi <= (others => '0');

end RTL;
