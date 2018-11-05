package ats.metro;

import java.util.Collection;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
import java.util.Spliterator;
import java.util.function.Consumer;
import java.util.function.Predicate;
import java.util.function.UnaryOperator;
import java.util.stream.Stream;

class MetroLogicList implements List<MetroSequence> {
	private static final String MSG_NOT_IMPLEMENTED_0 = "Not implemented. Use metro.putLogic() method";
	private static final String MSG_NOT_IMPLEMENTED = "not implemented.";
	final List<MetroTrack> object;
	public MetroLogicList(List<MetroTrack> object) {
		super();
		this.object = object;
	}

	public void forEach(Consumer<? super MetroSequence> action) {
		object.forEach((e)->{
			action.accept( e.sequence );
		});
	}

	public int size() {
		return object.size();
	}

	public boolean isEmpty() {
		return object.isEmpty();
	}

	public boolean contains(Object o) {
		for ( MetroTrack track : object  ) {
			if ( o.equals( track.sequence ) ) {
				return true;
			}
		}
		return false;
	}

	public Iterator<MetroSequence> iterator() {
		Iterator<MetroTrack> i = object.iterator();
		return new Iterator<MetroSequence>() {
			@Override
			public MetroSequence next() {
				return i.next().sequence;
			}
			
			@Override
			public boolean hasNext() {
				return i.hasNext();
			}
		};
	}

	public Object[] toArray() {
		Object[] result = new Object[ object.size() ];
		for ( int i=0; i<object.size(); i++  ) {
			result[i] = object.get(i).sequence;
		}
		return result;
	}

	public <T> T[] toArray(T[] a) {
		for ( int i=0; i<object.size(); i++  ) {
			a[i] = (T)object.get(i).sequence;
		}
		return a;

	}

	public boolean add(MetroSequence e) {
		throw new RuntimeException( MSG_NOT_IMPLEMENTED_0);
	}

	public boolean remove(Object o) {
		return object.remove(o);
	}

	public boolean containsAll(Collection<?> c) {
		for ( Object o : c  ) {
			if ( ! this.contains(o) ) {
				return false;
			}
		}
		return true;
	}

	public boolean addAll(Collection<? extends MetroSequence> c) {
		throw new RuntimeException( MSG_NOT_IMPLEMENTED_0);
	}

	public boolean addAll(int index, Collection<? extends MetroSequence> c) {
		throw new RuntimeException( MSG_NOT_IMPLEMENTED_0);
	}

	public boolean removeAll(Collection<?> c) {
		throw new RuntimeException( MSG_NOT_IMPLEMENTED_0);
	}

	public boolean retainAll(Collection<?> c) {
		throw new RuntimeException( MSG_NOT_IMPLEMENTED_0);
	}

	public void replaceAll(UnaryOperator<MetroSequence> operator) {
		throw new RuntimeException( MSG_NOT_IMPLEMENTED_0);
	}

	public boolean removeIf(Predicate<? super MetroSequence> filter) {
		return object.removeIf( (e)->filter.test(e.sequence) );
	}

	public void sort(Comparator<? super MetroSequence> c) {
		object.sort( new Comparator<MetroTrack>() {
			@Override
			public int compare(MetroTrack o1, MetroTrack o2) {
				return c.compare( o1.sequence , o2.sequence );
			}
		});
	}

	public void clear() {
		object.clear();
	}

	public boolean equals(Object o) {
		return object.equals(o);
	}

	public int hashCode() {
		return object.hashCode();
	}

	public MetroSequence get(int index) {
		return object.get(index).sequence;
	}

	public MetroSequence set(int index, MetroSequence element) {
		throw new RuntimeException( MSG_NOT_IMPLEMENTED_0);
	}

	public void add(int index, MetroSequence element) {
		throw new RuntimeException( MSG_NOT_IMPLEMENTED_0);
	}

	public Stream<MetroSequence> stream() {
		throw new RuntimeException( MSG_NOT_IMPLEMENTED );
	}

	public MetroSequence remove(int index) {
		return object.remove(index).sequence;
	}

	public Stream<MetroSequence> parallelStream() {
		throw new RuntimeException( MSG_NOT_IMPLEMENTED );
	}

	public int indexOf(Object o) {
		return object.indexOf( ((MetroSequence)o).trackInfo );
	}

	public int lastIndexOf(Object o) {
		return object.lastIndexOf( ((MetroSequence)o).trackInfo );
	}

	public ListIterator<MetroSequence> listIterator() {
		return new MetroLogicListIterator( object.listIterator() );
	}

	public ListIterator<MetroSequence> listIterator(int index) {
		return new MetroLogicListIterator( object.listIterator( index ) );
	}

	public List<MetroSequence> subList(int fromIndex, int toIndex) {
		throw new RuntimeException( MSG_NOT_IMPLEMENTED );
	}

	public Spliterator<MetroSequence> spliterator() {
		throw new RuntimeException( MSG_NOT_IMPLEMENTED );
	}

	
	private final class MetroLogicListIterator implements ListIterator<MetroSequence> {
		private final ListIterator<MetroTrack> listIterator;
		private MetroLogicListIterator(ListIterator<MetroTrack> listIterator) {
			this.listIterator = listIterator;
		}
		@Override
		public void set(MetroSequence e) {
			throw new RuntimeException( MSG_NOT_IMPLEMENTED );
		}

		@Override
		public void remove() {
			listIterator.remove();
		}

		@Override
		public int previousIndex() {
			return listIterator.previousIndex();
		}

		@Override
		public MetroSequence previous() {
			return listIterator.previous().sequence;
		}

		@Override
		public int nextIndex() {
			return listIterator.nextIndex();
		}

		@Override
		public MetroSequence next() {
			return listIterator.next().sequence;
		}

		@Override
		public boolean hasPrevious() {
			return listIterator.hasPrevious();
		}

		@Override
		public boolean hasNext() {
			return listIterator.hasNext();
		}

		@Override
		public void add(MetroSequence e) {
			throw new RuntimeException( MSG_NOT_IMPLEMENTED );
		}
	}
}