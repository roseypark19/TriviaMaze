package utilities;

import java.io.File;
import java.util.HashMap;
import java.util.Map;

import javax.sound.sampled.AudioInputStream;
import javax.sound.sampled.AudioSystem;
import javax.sound.sampled.Clip;
import javax.sound.sampled.FloatControl;

import model.SoundType;

public class SoundUtilities {
	
	private static final Map<SoundType, Clip> SOUNDS = collectSounds();

	public static void play(final SoundType theType) {
		final Clip clip = SOUNDS.get(theType);
		clip.setFramePosition(0);
		if (theType.equals(SoundType.BACKGROUND) || theType.equals(SoundType.TITLE)) {
			clip.loop(Clip.LOOP_CONTINUOUSLY);
		}
		clip.start();
	}
	
	public static void stop(final SoundType theType) {
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
			}
		}
		return sounds;
	}

	public static boolean isPlaying(final SoundType theType) {
		return SOUNDS.get(theType).isActive();
	}
	
	public static void changeVolume(final int theNewVolume) {;
		for (final SoundType sT : SOUNDS.keySet()) {
			final FloatControl fC = (FloatControl) SOUNDS.get(sT).getControl(FloatControl.Type.MASTER_GAIN);
			fC.setValue(20.0f * (float) Math.log10(theNewVolume/100.0));
		}
	}
}
