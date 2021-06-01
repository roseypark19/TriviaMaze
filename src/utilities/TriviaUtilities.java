/*
 * TriviaUtilities.java
 * TCSS360 - Trivia Maze
 * Spring 2021
 */

package utilities;

import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;

import org.sqlite.SQLiteDataSource;

import model.Trivia;
import model.TriviaFactory;
import model.TriviaType;

/**
 * TriviaUtilities is a class for accessing and providing trivia data from a specified
 * SQLite database source.
 * 
 * @author Parker Rosengreen, Rebekah Parkhurst, Artem Potafiy
 * @version 31 May 2021
 */
public class TriviaUtilities {

	/** The list of trivia retrieved from the SQLite database source */
	private static final List<Trivia> TRIVIA_LIST = new LinkedList<>();

	/**
	 * Returns a randomly shuffled copy of the trivia list. If the list has not yet been
	 * created, then the SQLite database source is accessed and a new trivia list is populated.
	 * 
	 * @return the randomly shuffled list of trivia
	 */
	public static List<Trivia> getTriviaList() {
		if (TRIVIA_LIST.isEmpty()) {
			SQLiteDataSource dataSource = accessDataSource();
			addTriviaToList(dataSource);
		}
		final List<Trivia> trivCopy = getTriviaListCopy();
		Collections.shuffle(trivCopy);
		return trivCopy;
	}

	/**
	 * Creates and returns a copy of the trivia list.
	 * 
	 * @return the trivia list copy
	 */
	private static List<Trivia> getTriviaListCopy() {
		final List<Trivia> copy = new LinkedList<>();
		for (final Trivia triv : TRIVIA_LIST) {
			copy.add(triv.copy());
		}
		return copy;
	}

	/**
	 * Retrieves all trivia from the SQLite database source and adds them to the trivia list.
	 * 
	 * @param theDataSource the SQLite database source
	 */
	private static void addTriviaToList(final SQLiteDataSource theDataSource) {
		ResultSet rs = null;
		try (Connection conn = theDataSource.getConnection(); Statement stmt = conn.createStatement();) {
			final String query = "SELECT * FROM ";
			rs = stmt.executeQuery(query + TriviaType.MULTICHOICE.toString());
			addMultiChoiceToList(rs);
			rs = stmt.executeQuery(query + TriviaType.TRUEFALSE.toString());
			addTrueFalseToList(rs);
			rs = stmt.executeQuery(query + TriviaType.SHORTANSWER.toString());
			addShortAnswerToList(rs);
		} catch (final SQLException ex) {
			System.err.println("Difficulties retrieving trivia from database!");
			System.exit(1);
		}
	}

	/**
	 * Adds short answer trivia from SQLite database source to the trivia list.
	 * 
	 * @param theSet the result set corresponding to the short answer query
	 * @throws SQLException if an access error occurs with theSet
	 */
	private static void addShortAnswerToList(final ResultSet theSet) throws SQLException {
		final String queryQuestion = "question";
		final String queryCorrect = "correct";
		while (theSet.next()) {
			final String question = theSet.getString(queryQuestion);
			final String correct = theSet.getString(queryCorrect);
			TRIVIA_LIST.add(TriviaFactory.createTrivia(correct, question, TriviaType.SHORTANSWER, null));
		}
	}

	/**
	 * Adds multiple choice trivia from SQLite database source to the trivia list.
	 * 
	 * @param theSet the result set corresponding to the multiple choice query
	 * @throws SQLException if an access error occurs with theSet
	 */
	private static void addMultiChoiceToList(final ResultSet theSet) throws SQLException {
		final String queryQuestion = "question";
		final String queryCorrectLetter = "correctletter";
		final String queryAnswers = "answers";
		while (theSet.next()) {
			final String question = theSet.getString(queryQuestion);
			final String correct = theSet.getString(queryCorrectLetter);
			final String answers = theSet.getString(queryAnswers);
			final String[] parsedAnswers = parseMultiChoiceAnswers(answers);
			TRIVIA_LIST.add(TriviaFactory.createTrivia(correct, question, TriviaType.MULTICHOICE, parsedAnswers));
		}
	}

	/**
	 * Adds true/false trivia from SQLite database source to the trivia list.
	 * 
	 * @param theSet the result set corresponding to the true/false query
	 * @throws SQLException if an access error occurs with theSet
	 */
	private static void addTrueFalseToList(final ResultSet theSet) throws SQLException {
		final String queryQuestion = "question";
		final String queryCorrect = "correctbool";
		while (theSet.next()) {
			final String question = theSet.getString(queryQuestion);
			final String correct = theSet.getString(queryCorrect);
			TRIVIA_LIST.add(TriviaFactory.createTrivia(correct, question, TriviaType.TRUEFALSE, null));
		}
	}

	/**
	 * Parses multiple choice answers using a provided query string of answers.
	 * 
	 * @param theQueryString the query string of answers
	 * @return an array of individually parsed answers
	 */
	private static String[] parseMultiChoiceAnswers(final String theQueryString) {
		String[] answers = theQueryString.split(",");
		for (int i = 0; i < answers.length; i++) {
			answers[i] = answers[i].trim();
		}
		return answers;
	}

	/**
	 * Retrieves the data source for accessing the SQLite database containing trivia.
	 * 
	 * @return SQLiteDataSource the data source for accessing the SQLite database.
	 */
	private static SQLiteDataSource accessDataSource() {
		SQLiteDataSource dataSource = null;
		try {
			dataSource = new SQLiteDataSource();
			dataSource.setUrl("jdbc:sqlite:trivia.db");
		} catch (final Exception ex) {
			System.err.println("Difficulties creating SQLiteDataSource!");
			System.exit(1);
		}
		return dataSource;
	}

}
