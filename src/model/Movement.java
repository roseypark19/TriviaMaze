package model;

public enum Movement {
	
	UP('W'),
	
	LEFT('A'),
	
	DOWN('S'),
	
	RIGHT('D');
	
	private final char myLetter;
	
	Movement(final char theLetter) {
		myLetter = theLetter;
	}
	
	@Override
	public String toString() {
		return String.valueOf(myLetter);
	}
	
	public Movement getOpposite() {
		Movement opposite = null;
		switch (myLetter) {
			case 'W':
				opposite = DOWN;
				break;
			case 'A':
				opposite = RIGHT;
				break;
			case 'S':
				opposite = UP;
				break;
			case 'D':
				opposite = LEFT;
				break;
		}
		return opposite;
	}
	
	public static Movement valueof(final char theChar) {
		Movement move = DOWN;
		for (final Movement movement : Movement.values()) {
			if (movement.myLetter == theChar) {
				move = movement;
				break;
			}
		}
		return move;
	}

}
