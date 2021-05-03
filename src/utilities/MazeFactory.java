package utilities;

import java.awt.Point;
import java.util.ArrayDeque;
import java.util.Collections;
import java.util.Deque;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import model.MazeTile;
import model.Movement;

public class MazeFactory {
	
	private static final int SIZE = 14;
	private static final int START = 68;
	private static final int FINISH = 740;
	
	public static MazeTile[][] getNewMaze() {
		final MazeTile[][] maze = new MazeTile[SIZE][SIZE];
		final int startX = START;
		final int startY = startX;
		int currX = startX;
		int currY = startY;
		for (int row = 0; row < SIZE; row++) {
			for (int col = 0; col < SIZE; col++) {
				maze[row][col] = new MazeTile(currX, currY);
				currX += MazeTile.SIZE;
			}
			currX = startX;
			currY += MazeTile.SIZE;
		}
		return maze;
	}
	
	public static Map<Point, MazeTile> getMaze() {
		final Map<Point, MazeTile> maze = new HashMap<>();
		
//		maze.put(new Point(START, START), new MazeTile(START, START));
//		maze.put(new Point(FINISH, FINISH), new MazeTile(FINISH, FINISH));
		
		
		final Deque<Point> pointStack = new ArrayDeque<>();
		final List<Movement> moves = new LinkedList<>();
		for (final Movement move : Movement.values()) {
			moves.add(move);
		}
		Point curr = new Point(START, START);
		while (curr.getX() != FINISH || curr.getY() != FINISH) {
//			System.out.println(curr.getX());
//			System.out.println(curr.getY());
			maze.put(curr, new MazeTile(curr.getX(), curr.getY()));
			pointStack.push(curr);
			Collections.shuffle(moves);
			boolean moveComplete = false;
			int tries = 0;
			do {
				final Movement nextMove = moves.remove(0);
				final Point tryPoint = maze.get(curr).getPointForMovement(nextMove);
				if (!maze.containsKey(tryPoint) && isValidPoint(tryPoint)) {
					curr = tryPoint;
					moveComplete = true;
				} else {
					tries++;
				}
				moves.add(nextMove);
			} while (!moveComplete && tries < moves.size());
			if (!moveComplete) {
				pointStack.pop();
				curr = pointStack.pop();
			}
		}
		maze.put(curr, new MazeTile(curr.getX(), curr.getY()));
		return maze;
	}
	
	private static boolean isValidPoint(final Point thePoint) {
		return thePoint.getX() >= START && thePoint.getY() >= START &&
			   thePoint.getX() <= FINISH && thePoint.getY() <= FINISH;
	}
	
//	public static void createNewMaze() {
//		final Set<MazeTile> maze = new HashSet<>();
//		final MazeTile current = new MazeTile(START, START);
//		maze.add(current);
//		System.out.println(maze.contains(new MazeTile(START, START)));
//		final Random rand = new Random();
//		final Deque<MazeTile> tileStack = new ArrayDeque<>();
//		final List<Movement> moves = new LinkedList<>();
//		for (final Movement move : Movement.values()) {
//			moves.add(move);
//		}
//		
//		final MazeTile current = new MazeTile(START, START);
//		double currX = START;
//		double currY = START;
//		
//		while (current.getX() != FINISH || current.getY() != FINISH) {
//			maze.add(current);
//			tileStack.push(current);
//			Collections.shuffle(moves);
//			final Movement firstTry = moves.get(0);
//			
//		}
		
		
		
//		return maze;
//	}
	
//	public static List<MazeTile> getMaze() {
//		final List<MazeTile> maze = new ArrayList<>();
//		try (final Scanner input = new Scanner(new File("testMaze.txt"))) {
//			
//			final int startX = START;
//			final int startY = startX;
//			int currX = startX;
//			int currY = startY;
//			while (input.hasNextLine()) {
//				final String row = input.nextLine();
//				for (int index = 0; index < row.length(); index++) {
//					if (row.charAt(index) == TILE) {
//						maze.add(new MazeTile(currX, currY));
//					}
//					currX += MazeTile.SIZE;
//				}
//				currX = startX;
//				currY += MazeTile.SIZE;
//			}
//			
//		} catch (final FileNotFoundException ex) {
//			System.out.println("Could not open maze file!");
//		}
//		return maze;
//	}

//	private static int[][] createAdjMatrix() {
//		final int[][] adjMatrix = new int[SIZE * SIZE][SIZE * SIZE];
//		final Random rand = new Random();
//		int count = 0;
//		for (int row = 0; row < adjMatrix.length; row++) {
//			for (int col = 0; col <= row; col++) {
//				if (row != col && (row / SIZE == col / SIZE || 
//						           row % SIZE == col % SIZE)) {
//					adjMatrix[row][col] = rand.nextInt(SIZE * SIZE) + 1;
//					count++;
//				} else {
//					adjMatrix[row][col] = 0;
//				}
//				adjMatrix[col][row] = adjMatrix[row][col];
//			}
//		}
//		return adjMatrix;
//	}

}
