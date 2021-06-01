/*
 * SoundUtilities.java
 * TCSS360 - Trivia Maze
 * Spring 2021
 */

package utilities;

import java.io.File;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

import javax.sound.sampled.AudioInputStream;
import javax.sound.sampled.AudioSystem;
import javax.sound.sampled.Clip;
import javax.sound.sampled.FloatControl;

import model.SoundType;

/**
 * SoundUtilities is a class which controls all audio input and output for the Maze Hops game.
 * 
 * @author Parker Rosengreen, Rebekah Parkhurst, Artem Potafiy
 * @version 31 May 2021
 */
public class SoundUtilities {
	
	/** The mapping of each sound type to its corresponding audio clip */
	private static final Map<SoundType, Clip> SOUNDS = collectSounds();

	/**
	 * Plays the audio clip corresponding to the provided sound type.
	 * 
	 * @param theType the sound type to be played
	 * @throws NullPointerException if theType is null
	 */
	public static void play(final SoundType theType) {
		Objects.requireNonNull(theType, "Sound types must be non-null!");
		if (!isPlaying(theType)) {
			final Clip clip = SOUNDS.get(theType);
			clip.setFramePosition(0);
			if (theType.equals(SoundType.BACKGROUND) || theType.equals(SoundType.TITLE)) {
				clip.loop(Clip.LOOP_CONTINUOUSLY);
			}
			clip.start();
		}
	}
	
	/**
	 * Stops the audio clip corresponding to the provided sound type.
	 * 
	 * @param theType the sound type to be stopped
	 * @throws NullPointerException if theType is null
	 */
	public static void stop(final SoundType theType) {
		Objects.requireNonNull(theType, "Sound types must be non-null!");
		SOUNDS.get(theType).stop();
	}
	
	/**
	 * Creates and supplies the mapping of each sound type to its corresponding audio clip.
	 * 
	 * @return the mapping of each sound type to its audio clip
	 */
	private static Map<SoundType, Clip> collectSounds() {
		final Map<SoundType, Clip> sounds = new HashMap<>();
		for (final SoundType soundType : SoundType.values()) {
			try {
				final AudioInputStream aIS = 
						  AudioSystem.getAudioInputStream(new File(soundType.toString()));
				final Clip clip = AudioSystem.getClip();
				clip.open(aIS);
				sounds.put(soundType, clip);
			} catch (final Exception ex) {
				System.err.println("Difficulties opening sounds!");
				System.exit(1);
			}
		}
		return sounds;
	}

	/**
	 * Indicates whether the audio clip corresponding to the provided sound type is currently
	 * playing.
	 * 
	 * @param theType the sound type in question
	 * @return true if the corresponding audio clip is currently active, false otherwise
	 * @throws NullPointerException if theType is null
	 */
	public static boolean isPlaying(final SoundType theType) {
		Objects.requireNonNull(theType, "Sound types must be non-null!");
		return SOUNDS.get(theType).isActive();
	}
	
	/**
	 * Changes the volume of all audio clips to that of the supplied new volume. Note that 
	 * valid volumes are those between 0 and 100 inclusive.
	 * 
	 * @param theNewVolume the new volume to be assigned
	 * @throws IllegalArgumentException if theNewVolume is not between 0 and 100 inclusive
	 */
	public static void changeVolume(final int theNewVolume) {
		if (theNewVolume < 0 || theNewVolume > 100) {
			throw new IllegalArgumentException("Volumes must be between 0 and 100 inclusive!");
		}
		for (final SoundType sT : SOUNDS.keySet()) {
			final Clip clip = SOUNDS.get(sT);
			final FloatControl fC = (FloatControl) clip.getControl(FloatControl.Type.MASTER_GAIN);
			fC.setValue(20.0f * (float) Math.log10(theNewVolume / 100.0));
		}
	}
}
