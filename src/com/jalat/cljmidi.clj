
(ns com.jalat.cljmidi
  (:import (javax.sound.midi MidiSystem MidiUnavailableException
			     MidiDevice MidiDevice$Info
			     Receiver Transmitter Synthesizer Sequencer
			     MidiMessage ShortMessage SysexMessage MetaMessage)))

(defn get-mididevices
  "Returns a list of hashes with info about the midi devices available."
  []
  (map (fn [device]
	 {:name (.getName device)
	  :vendor (.getVendor device)
	  :version (.getVersion device)
	  :description (.getDescription device)
	  :device (. MidiSystem getMidiDevice device)})
       (. MidiSystem getMidiDeviceInfo)))

(defn filter-mididevices [class device-infos]
  "returns a list of midi devices of the Class class from a map of midi devices"
  (filter (fn [device-info]
	    (instance? class (:device device-info)))
	  device-infos))

(def midi-shortmessage-status {ShortMessage/ACTIVE_SENSING :active-sensing
                               ShortMessage/CONTINUE :continue
                               ShortMessage/END_OF_EXCLUSIVE :end-of-exclusive
                               ShortMessage/MIDI_TIME_CODE :midi-time-code
                               ShortMessage/SONG_POSITION_POINTER :song-position-pointer
                               ShortMessage/SONG_SELECT :song_select
                               ShortMessage/START :start
                               ShortMessage/STOP :stop
                               ShortMessage/SYSTEM_RESET :system-reset
                               ShortMessage/TIMING_CLOCK :timing-clock
                               ShortMessage/TUNE_REQUEST :tune-request})

(def midi-sysexmessage-status {SysexMessage/SYSTEM_EXCLUSIVE :system-exclusive
                               SysexMessage/SPECIAL_SYSTEM_EXCLUSIVE :special-system-exclusive})

(def midi-shortmessage-command {ShortMessage/CHANNEL_PRESSURE :channel-pressure
                                ShortMessage/CONTROL_CHANGE :control-change
                                ShortMessage/NOTE_OFF :note-off
                                ShortMessage/NOTE_ON :note-on
                                ShortMessage/PITCH_BEND :pitch-bend
                                ShortMessage/POLY_PRESSURE :poly-pressure
                                ShortMessage/PROGRAM_CHANGE :program-change})

(def key-names [:C :C# :D :D#  :E :F :F# :G :G# :A :A# :B])

(defn keyname 
  "Given a midi note, returns the name of the note/key"
  [index]
  (nth (cycle key-names) index))

(defn calculate-14-bit-value
  "Calculates the the 14 bit value given two integers 
representing the high and low parts of a 14 bit value."
  [lower higher]
  (bit-or (bit-and lower 0x7f)
    (bit-shift-left (bit-and higher 0x7f) 7)))

(defn- decode-midi-command 
  "Takes the data of a midi-command and returns a hashmap of the message"
  [command channel data1 data2]
  (cond (#{:note-on :note-off} command)
	{:command command :channel channel :key (keyname data1)
	 :octave (int (/ data1 12)) :velocity data2}
	
	(#{:channel-pressure :poly-pressure} command)
	{:command command :channel channel :key (keyname data1) 
	 :octave (int (/ data1 12)) :pressure data2}

	(= :control-change command)
	{:command command :channel channel :change data1 :value data2}

	(= :program-change command)
	{:command command :chanel channel :change data1}

	(= :pitch-bend command)
	{:command command :channel channel
	 :change (calculate-14-bit-value data1 data2)}))

(defmulti decode-midi-message class)
(defmethod decode-midi-message javax.sound.midi.ShortMessage [message]
  (let [status (midi-shortmessage-status (. message getStatus))
        command (midi-shortmessage-command (. message getCommand))
        channel (inc (. message getChannel))
        data1 (. message getData1)
        data2 (. message getData2)]
    (cond command (decode-midi-command command channel data1 data2)
	  status  {:status status}
	  :else	  {:unknown-status (. message getStatus)
		   :unknown-command (. message getCommand)
		   :byte1 data1 :byte2 data2})))

(defmethod decode-midi-message SysexMessage [message]
  (let [bytes (. message getData)]
    {:status (midi-sysexmessage-status (. message getStatus))
     :data bytes}))

(defn midi-input-callback
  "Sets up a callback to f with a map representing a midi message"
  [transmitter f]
  (let [receiver (proxy [Receiver] []
		   (close [] nil)
		   (send [message timestamp]
			 (f (assoc (decode-midi-message message)
		    	           :timestamp timestamp))))]
    (. transmitter setReceiver receiver)
    (. transmitter open)
    transmitter))

(defn midi-input-collection
  "Takes a transmitter as the argument and sets up a receiver that puts
all incoming messages into a ref to a sequence. Returns the ref"
  [transmitter]
  (let [midi-data (ref ())
        receiver (proxy [Receiver] []
		   (close [] nil)
		   (send [message timestamp]
			 (dosync
			  (alter midi-data
				 conj (assoc (decode-midi-message message)
					:timestamp timestamp)))))]
    (. transmitter setReceiver receiver)
    (. transmitter open)
    midi-data))

(defn setup-midi-agent
  "Sets up and agent and sends 'handler-function' to it whenever a message
arrives from the transmitter"
  [transmitter handler-function]
  (let [midi-agent (agent {:transmitter transmitter
                           :beat-stamp 0
                           :beat-gap 0
                           :timing-clock-queue []
                           :last-message-timestamp 0})]
    (midi-input-callback transmitter
			 (fn [message-map]
			   (send midi-agent handler-function message-map)))
    midi-agent))

(defn flush-old-midi-messages
  "removes all messages from the agents message queue older than the timestamp"
  [agent timestamp]
  (assoc agent
    :messages (filter #(< timestamp (:timestamp %)) (:messages agent))))

(defn get-time 
  "get a timestamp from the device"
  [device]
  (. device getMicrosecondPosition))
