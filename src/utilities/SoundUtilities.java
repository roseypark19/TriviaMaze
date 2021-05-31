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

public class SoundUtilities {
	
	private static final Map<SoundType, Clip> SOUNDS = collectSounds();

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
	
	public static void stop(final SoundType theType) {
		Objects.requireNonNull(theType, "Sound types must be non-null!");
		SOUNDS.get(theType).stop();
	}
	
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

	public static boolean isPlaying(final SoundType theType) {
		Objects.requireNonNull(theType, "Sound types must be non-null!");
		return SOUNDS.get(theType).isActive();
	}
	
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
