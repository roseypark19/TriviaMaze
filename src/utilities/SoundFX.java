package utilities;
import javax.sound.sampled.*;
import java.io.IOException;
import java.net.URL;
import java.io.File;

/**
 * 
 * @author Rebekah Parkhurst
 * adapted from https://gist.github.com/figengungor/5673813
 *
 */

public enum SoundFX {
	
	GAME_MUSIC("/triviaMaze/LoopMusic.wav"),
	CORRECT("/triviaMaze/correct.wav"),
	INCORRECT("/triviaMaze/fail.wav"),
	WATER("water.wav"),
	WIN_GAME("/triviaMaze/wingame.wav");
	
	private final String mySFX;

	SoundFX(String fileName) {
		mySFX = fileName;
	}
	
//	@Override
//	public String toString() {
//		switch (sfx) {
//			case GAME_MUSIC: return "/triviaMaze/LoopMusic.wav";
//			case CORRECT: return "/triviaMaze/correct.wav";
//			case INCORRECT: return "/triviaMaze/fail.wav";
//			case WATER: return "/triviaMaze/water.wave";
//			case WIN_GAME: return "/triviaMaze/wingame.wave";
//		}
//		return null;
//	}

}
