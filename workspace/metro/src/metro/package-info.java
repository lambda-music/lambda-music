/*
 * Metro Musical Sequencing Framework written by Atsushi Oka 
 * Copyright 2018 Atsushi Oka
 *
 * This file is part of Metro Musical Sequencing Framework. 
 * 
 * Metro Musical Sequencing Framework is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * Metro Musical Sequencing Framework is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with Metro Musical Sequencing Framework.  If not, see <https://www.gnu.org/licenses/>.
 */
/**
 * Metro is a framework which sends/receives MIDI messages via JACK Audio
 * Connection Kit. The major strength of Metro is that it is designed to compose
 * music on-the-fly. JACK processes MIDI data per a frame; the client
 * applications are required to split all data down to small chunks. Metro
 * provides a mechanism to buffer the client MIDI data and send it to JACK
 * frame-by-frame.
 * 
 * Metro also provides a mechanism to compose/decompose MIDI messages. 
 * 
 */
package metro;
