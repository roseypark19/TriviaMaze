package utilities;
import javax.sound.sampled.*;
import java.io.IOException;
import java.io.File;

import utilities.SoundFX;

public class SoundPlayer {
	
	private Clip clip;
	private AudioInputStream audioInputStream;
	
	public void setFile(final String theSound) {
		try {
			File file = new File(theSound);
			audioInputStream = AudioSystem.getAudioInputStream(file);
			clip = AudioSystem.getClip();
			clip.open(audioInputStream);
		} catch (UnsupportedAudioFileException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		} catch (LineUnavailableException e) {
			e.printStackTrace();
		}
	}
	
	public void play() {
		clip.setFramePosition(0);
		clip.start();
	}
	
	public void stop() {
		clip.stop();
	}
	
}
