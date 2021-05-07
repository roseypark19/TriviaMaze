package utilities;

import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

import javax.imageio.ImageIO;
import model.Movement;

public class SpriteUtilities {
	
	public static Map<Movement, BufferedImage[]> getPlayerSprites() {
		final Map<Movement, BufferedImage[]> playerSprites = new HashMap<>();
		try {
			final BufferedImage[] sprites = new BufferedImage[16];
			for (int i = 1; i <= sprites.length; i++) {
				sprites[i-1] = ImageIO.read(new File(String.format("image%d.png", i)));
			}
			
			playerSprites.put(Movement.DOWN, Arrays.copyOfRange(sprites, 0, 4));
			playerSprites.put(Movement.UP, Arrays.copyOfRange(sprites, 4, 8));
			playerSprites.put(Movement.LEFT, Arrays.copyOfRange(sprites, 8, 12));
			playerSprites.put(Movement.RIGHT, Arrays.copyOfRange(sprites, 12, 16));
			
		} catch (final IOException ex) {
			System.out.println("Could not load sprite image!");
		}
		return playerSprites;
	}
	
	public static BufferedImage getGrass() {
		BufferedImage grass = null;
		try {
			grass = ImageIO.read(new File("Grass.jpg"));
		} catch (final IOException ex) {
			System.out.println("Could not load grass image!");
		}
		return grass;
	}

}
