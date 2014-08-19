#!/usr/bin/env ruby
# -*- coding: utf-8 -*-
require File.dirname(__FILE__) + '/../../test_helper'

unless Sass::Util.ruby1_8?
  class MultibyteStringScannerTest < MiniTest::Test
    def setup
      @scanner = Sass::Util::MultibyteStringScanner.new("cölorfül")
    end

    def test_initial
      assert_scanner_state 0, 0, nil, nil
    end

    def test_check
      assert_equal 'cö', @scanner.check(/../)
      assert_scanner_state 0, 0, 2, 3
      assert_equal 0, @scanner.pos
      assert_equal 0, @scanner.pos
      assert_equal 2, @scanner.matched_size
      assert_equal 3, @scanner.byte_matched_size
    end

    def test_check_until
      assert_equal 'cölorfü', @scanner.check_until(/f./)
      assert_scanner_state 0, 0, 2, 3
    end

    def test_getch
      assert_equal 'c', @scanner.getch
      assert_equal 'ö', @scanner.getch
      assert_scanner_state 2, 3, 1, 2
    end

    def test_match?
      assert_equal 2, @scanner.match?(/../)
      assert_scanner_state 0, 0, 2, 3
    end

    def test_peek
      assert_equal 'cö', @scanner.peek(2)
      assert_scanner_state 0, 0, nil, nil
    end

    def test_rest_size
      assert_equal 'cö', @scanner.scan(/../)
      assert_equal 6, @scanner.rest_size
    end

    def test_scan
      assert_equal 'cö', @scanner.scan(/../)
      assert_scanner_state 2, 3, 2, 3
    end

    def test_scan_until
      assert_equal 'cölorfü', @scanner.scan_until(/f./)
      assert_scanner_state 7, 9, 2, 3
    end

    def test_skip
      assert_equal 2, @scanner.skip(/../)
      assert_scanner_state 2, 3, 2, 3
    end

    def test_skip_until
      assert_equal 7, @scanner.skip_until(/f./)
      assert_scanner_state 7, 9, 2, 3
    end

    def test_set_pos
      @scanner.pos = 7
      assert_scanner_state 7, 9, nil, nil
      @scanner.pos = 6
      assert_scanner_state 6, 7, nil, nil
      @scanner.pos = 1
      assert_scanner_state 1, 1, nil, nil
    end

    def test_reset
      @scanner.scan(/../)
      @scanner.reset
      assert_scanner_state 0, 0, nil, nil
    end

    def test_scan_full
      assert_equal 'cö', @scanner.scan_full(/../, true, true)
      assert_scanner_state 2, 3, 2, 3

      @scanner.reset
      assert_equal 'cö', @scanner.scan_full(/../, false, true)
      assert_scanner_state 0, 0, 2, 3

      @scanner.reset
      assert_nil @scanner.scan_full(/../, true, false)
      assert_scanner_state 2, 3, 2, 3

      @scanner.reset
      assert_nil @scanner.scan_full(/../, false, false)
      assert_scanner_state 0, 0, 2, 3
    end

    def test_search_full
      assert_equal 'cölorfü', @scanner.search_full(/f./, true, true)
      assert_scanner_state 7, 9, 2, 3

      @scanner.reset
      assert_equal 'cölorfü', @scanner.search_full(/f./, false, true)
      assert_scanner_state 0, 0, 2, 3

      @scanner.reset
      assert_nil @scanner.search_full(/f./, true, false)
      assert_scanner_state 7, 9, 2, 3

      @scanner.reset
      assert_nil @scanner.search_full(/f./, false, false)
      assert_scanner_state 0, 0, 2, 3
    end

    def test_set_string
      @scanner.scan(/../)
      @scanner.string = 'föóbâr'
      assert_scanner_state 0, 0, nil, nil
    end

    def test_terminate
      @scanner.scan(/../)
      @scanner.terminate
      assert_scanner_state 8, 10, nil, nil
    end

    def test_unscan
      @scanner.scan(/../)
      @scanner.scan_until(/f./)
      @scanner.unscan
      assert_scanner_state 2, 3, nil, nil
    end

    private

    def assert_scanner_state(pos, byte_pos, matched_size, byte_matched_size)
      assert_equal pos, @scanner.pos, 'pos'
      assert_equal byte_pos, @scanner.byte_pos, 'byte_pos'
      assert_equal matched_size, @scanner.matched_size, 'matched_size'
      assert_equal byte_matched_size, @scanner.byte_matched_size, 'byte_matched_size'
    end
  end
end
