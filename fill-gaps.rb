#!/usr/bin/ruby

# https://github.com/alopatindev/audacity-plugins
# Copyright (C) 2018  Alexander Lopatin
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

require 'optparse'
require 'tmpdir'

MIXER_FFMPEG = 'ffmpeg'.freeze
MIXER_SOX = 'sox'.freeze

def compute_duration(file)
  `ffprobe -v error -show_entries format=duration -of default=noprint_wrappers=1:nokey=1 #{file}`.to_f
end

def analyze_gaps(file, min_gap_duration)
  `ffmpeg -i #{file} -af silencedetect=n=-50dB:d=#{min_gap_duration} -f null - 2>> /dev/stdout`
    .split("\n")
    .select { |line| line.match(/silencedetect.*silence_(start|end)/) }
    .map { |line| line.sub(/.*silence_(start|end): /, '') }
    .map { |line| line.sub(/ \| silence_duration: [0-9.]*/, '') }
    .map(&:to_f)
end

def compute_segments(temp_dir, options)
  sound_file = options[:input]
  background_file = options[:background]
  min_gap_duration = options[:min_gap_duration]

  sound_duration = compute_duration(sound_file)
  background_duration = compute_duration(background_file)

  new_segment_props = lambda { |from, to, is_gap|
    duration = to - from
    segment = { from: from, to: to, duration: duration, is_gap: is_gap }
    segment[:input] =
      if is_gap then { file: background_file, from: 0.0, to: duration }
      else { file: sound_file, from: from, to: to }
      end
    segment
  }

  new_gap_segments_props = lambda { |from, to|
    segment_duration = to - from
    gaps = (segment_duration / background_duration).ceil.to_i
    (0...gaps).map do |i|
      offset = i * background_duration
      gap_from = from + offset
      gap_to = [gap_from + background_duration, to].min
      is_gap = true
      new_segment_props.call(gap_from, gap_to, is_gap)
    end
  }

  gaps = analyze_gaps(sound_file, min_gap_duration)
  gaps_duplicated = gaps.map { |position| [position, position] }
  segments = [0.0] + gaps_duplicated.flatten + [sound_duration]

  segment_props = segments
                  .each_slice(2)
                  .map
                  .with_index do |(from, to), index|
    is_gap = index.odd?
    if is_gap then new_gap_segments_props.call(from, to)
    else [new_segment_props.call(from, to, is_gap)]
    end
  end

  segment_props
    .flatten
    .map
    .with_index do |segment, index|
      file = "tmp_#{index}.wav"
      segment.merge(file: File.join(temp_dir, file))
    end
end

def ffmpeg(input, output, args)
  `ffmpeg -y -hide_banner -loglevel quiet -i #{input} #{args} #{output}`
end

def ffmpeg_cut(input, output, from, to)
  ffmpeg(input, output, "-ss #{from} -to #{to} -c copy")
end

def add_silence_at_start(input, output, duration, channels)
  `sox --no-show-progress --no-dither #{input} #{output} pad #{duration} 0 channels #{channels}` # TODO: ffmpeg?
end

def split_files_to_segments(segments, temp_dir, options)
  overlap_duration = options[:overlap_duration]
  channels = options[:channels]
  fix_clicks = options[:fix_clicks]

  fade_duration = overlap_duration * 0.5
  fade_curve = 'log'

  unclick_fade_curve = 'log'
  click_duration = 0.160

  raw_file = File.join(temp_dir, 'tmp_raw.wav')
  unclicked_file = File.join(temp_dir, 'tmp_unclicked.wav')
  fade_file = File.join(temp_dir, 'tmp_fade.wav')

  segments.each do |segment|
    overlap_start = segment[:duration] - fade_duration
    input = segment[:input]

    click_start = segment[:duration] - click_duration
    unclick_fade_out_args = "afade=t=out:st=#{click_start}:d=#{click_duration}:curve=#{unclick_fade_curve}"

    fade_in_args = "afade=t=in:st=0:d=#{fade_duration}:curve=#{fade_curve}"
    fade_out_args = "afade=t=out:st=#{overlap_start}:d=#{overlap_duration}:curve=#{fade_curve}"

    ffmpeg_cut(input[:file], raw_file, input[:from], input[:to] + overlap_duration)

    if fix_clicks && !segment[:is_gap]
      ffmpeg(raw_file, unclicked_file, "-af #{unclick_fade_out_args}")
    else
      File.rename(raw_file, unclicked_file)
    end

    first_segment = segment[:from] <= 0.0
    if first_segment
      ffmpeg(unclicked_file, segment[:file], "-af #{fade_out_args}") # TODO: channels
    else
      ffmpeg(unclicked_file, fade_file, "-af #{fade_out_args},#{fade_in_args}")
      add_silence_at_start(fade_file, segment[:file], segment[:from], channels)
    end
  end
end

def mix_segments(segments, options)
  output_file = options[:output]
  mixer = options[:mixer]
  channels = options[:channels]

  case mixer
  when MIXER_FFMPEG
    inputs = segments.map { |f| '-i ' + f[:file] }.join(' ')
    `#{mixer} -y -hide_banner -loglevel quiet #{inputs} -filter_complex amix=inputs=#{segments.length} #{output_file}` # TODO: channels
  when MIXER_SOX
    # FIXME: sox applies normalization here, which is unexpected
    inputs = segments.map { |f| f[:file] }.join(' ')
    `#{mixer} --no-show-progress --no-dither --combine mix-power #{inputs} #{output_file} channels #{channels}`
  else
    raise "Unknown mixer \"#{mixer}\""
  end
end

def parse_options!(options)
  OptionParser.new do |opts|
    opts.banner = 'Usage: fill-gaps.rb -i sound.flac -b background.flac -o output.flac [other options]'
    opts.on('-i', '--input [sound.flac]', 'Input with gaps') { |i| options[:input] = i }
    opts.on('-b', '--background [background.flac]', 'Background sound for gaps') { |b| options[:background] = b }
    opts.on('-o', '--output [output.flac]', 'Output file. It will be overwritten if exists') do |o|
      options[:output] = o
    end
    opts.on('-t', '--transition [duration]', 'Segments overlap duration in seconds (default 0.5)') do |t|
      options[:overlap_duration] = t.to_f
    end
    opts.on('-g', '--min-gap [duration]', 'Minimum gap duration in seconds (default 0.05)') do |t|
      options[:min_gap_duration] = t.to_f
    end
    opts.on('-m', '--mixer [ffmpeg|sox]', 'Mixing tool (default ffmpeg)') { |m| options[:mixer] = m }
    opts.on('-c', '--channels [number]', 'Channels (default 1)') { |c| options[:channels] = c }
    opts.on('-d', '--temp-dir [path]', 'Temporary directory prefix (default /var/tmp)') { |d| options[:temp_dir_prefix] = d }
    opts.on('-f', '--fix-clicks [true|false]', 'Fix possible clicks before gaps') { |f| options[:fix_clicks] = f == 'true' }
  end.parse!

  p options

  raise OptionParser::MissingArgument if options[:input].nil?
  raise OptionParser::MissingArgument if options[:background].nil?
  raise OptionParser::MissingArgument if options[:output].nil?
end

options = {
  channels: 1,
  fix_clicks: true,
  min_gap_duration: 0.05,
  mixer: MIXER_FFMPEG,
  overlap_duration: 0.5,
  temp_dir_prefix: '/var/tmp'
}
parse_options!(options)

temp_dir_prefix = options[:temp_dir_prefix]
p temp_dir_prefix
Dir.mktmpdir(nil, temp_dir_prefix) do |temp_dir|
  segments = compute_segments(temp_dir, options)
  p segments
  split_files_to_segments(segments, temp_dir, options)
  mix_segments(segments, options)
end
