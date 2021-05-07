package utilities;

import java.awt.Point;
import java.util.ArrayDeque;
import java.util.Deque;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import model.MazeTile;
import model.Movement;

public class MazeSolver {
	
	public static boolean isSolveable(final Map<Point, MazeTile> theMaze) {
		boolean solveable = true;
		final Deque<Point> pointStack = new ArrayDeque<>();
		final Set<Point> visited = new HashSet<>();
		final List<Movement> moves = new LinkedList<>();
		for (final Movement move : Movement.values()) {
			moves.add(move);
		}
		final Point entry = MazeGenerator.getEntryPoint();
		final Point exit = MazeGenerator.getExitPoint();
		Point curr = entry;
		while ((curr.getX() != exit.getX() || curr.getY() != exit.getY()) && solveable) {
			visited.add(curr);
			boolean moveComplete = false;
			int tries = 0;
			do {
				final Movement nextMove = moves.remove(0);
				final Point tryPoint = theMaze.get(curr).getPointForMovement(nextMove);
				if (!visited.contains(tryPoint) && theMaze.containsKey(tryPoint)) {
					pointStack.push(curr);
					curr = tryPoint;
					moveComplete = true;
				} else {
					tries++;
				}
				moves.add(nextMove);
			} while (!moveComplete && tries < moves.size());
			if (!moveComplete) {
				curr = pointStack.pop();
			}
			solveable = pointStack.isEmpty() ? false : true;
		}
		return solveable;
	}
}
