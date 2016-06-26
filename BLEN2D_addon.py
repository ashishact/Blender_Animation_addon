# ##### BEGIN GPL LICENSE BLOCK #####
#
#  This program is free software; you can redistribute it and/or
#  modify it under the terms of the GNU General Public License
#  as published by the Free Software Foundation; either version 2
#  of the License, or (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program; if not, write to the Free Software Foundation,
#  Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
#
# ##### END GPL LICENSE BLOCK #####

bl_info = {
  "name": "BLEN2D HTML5 Animation Tool",
  "author": "Ashish Charan Tandi",
  "version": (1,0,0),
  "blender": (2, 74, 0),
  "location": "View3D > Toolbar > Animation ",
  "warning": "",
  "description": "BLEN2D HTML5 Animation Tool",
  "category": "Animation"
}

#************************************************************************************
#************************************************************************************

#for dict ordering
from operator import itemgetter
from collections import OrderedDict
import json

#from exporter
if "bpy" in locals():
    import importlib
    if "export_blen2d" in locals():
        importlib.reload(export_blen2d)

from bpy_extras.io_utils import ExportHelper, path_reference#, ImportHelper


#from shapekey
import math
from bpy_extras import view3d_utils
from bpy.types import Menu, Panel, UIList
from rna_prop_ui import PropertyPanel

#--------------------------------------------------------------------------------
#From Import Image as Planes
import bpy
from bpy.types import Menu, Operator, Header
import mathutils
import os
import collections

from bpy.props import (StringProperty,
                       BoolProperty,
                       EnumProperty,
                       IntProperty,
                       FloatProperty,
                       FloatVectorProperty,
                       CollectionProperty,
                       )

from bpy_extras.object_utils import AddObjectHelper, object_data_add
from bpy_extras.image_utils import load_image

# for animation2D 

from bpy.types import PropertyGroup


#************************************************************************************
#************************************************************************************

PPI = 100#Pixel per blender unit


#for blen2d export

# rotation_order = EnumProperty(

#   name="Rotation order",
#   description="Choose the export rotation order",
#   items=(('XYZ', "XYZ", "XYZ"),
#    ('XZY', "XZY", "XZY"),
#    ('YXZ', "YXZ", "YXZ"),
#    ('YZX', "YZX", "YZX"),
#    ('ZXY', "ZXY", "ZXY"),
#    ('ZYX', "ZYX", "ZYX"),
#    ),
#   default='XYZ')

# -----------------------------------------------------------------------------
# Global Vars

DEFAULT_EXT = "*"

EXT_FILTER = getattr(collections, "OrderedDict", dict)((
    (DEFAULT_EXT, ((), "All image formats", "Import all known image (or movie) formats.")),
    ("jpeg", (("jpeg", "jpg", "jpe"), "JPEG ({})", "Joint Photographic Experts Group")),
    ("png", (("png", ), "PNG ({})", "Portable Network Graphics")),
    ("tga", (("tga", "tpic"), "Truevision TGA ({})", "")),
    ("tiff", (("tiff", "tif"), "TIFF ({})", "Tagged Image File Format")),
    ("bmp", (("bmp", "dib"), "BMP ({})", "Windows Bitmap")),
    ("cin", (("cin", ), "CIN ({})", "")),
    ("dpx", (("dpx", ), "DPX ({})", "DPX (Digital Picture Exchange)")),
    ("psd", (("psd", ), "PSD ({})", "Photoshop Document")),
    ("exr", (("exr", ), "OpenEXR ({})", "OpenEXR HDR imaging image file format")),
    ("hdr", (("hdr", "pic"), "Radiance HDR ({})", "")),
    ("avi", (("avi", ), "AVI ({})", "Audio Video Interleave")),
    ("mov", (("mov", "qt"), "QuickTime ({})", "")),
    ("mp4", (("mp4", ), "MPEG-4 ({})", "MPEG-4 Part 14")),
    ("ogg", (("ogg", "ogv"), "OGG Theora ({})", "")),
))

# XXX Hack to avoid allowing videos with Cycles, crashes currently!
VID_EXT_FILTER = {e for ext_k, ext_v in EXT_FILTER.items() if ext_k in {"avi", "mov", "mp4", "ogg"} for e in ext_v[0]}

CYCLES_SHADERS = (
    ('BSDF_DIFFUSE', "Diffuse", "Diffuse Shader"),
    ('EMISSION', "Emission", "Emission Shader"),
    ('BSDF_DIFFUSE_BSDF_TRANSPARENT', "Diffuse & Transparent", "Diffuse and Transparent Mix"),
    ('EMISSION_BSDF_TRANSPARENT', "Emission & Transparent", "Emission and Transparent Mix")
)

# -----------------------------------------------------------------------------
# Misc utils.
def gen_ext_filter_ui_items():
    return tuple((k, name.format(", ".join("." + e for e in exts)) if "{}" in name else name, desc)
                 for k, (exts, name, desc) in EXT_FILTER.items())

#_________________________________________________________
def is_image_fn(fn, ext_key):
    if ext_key == DEFAULT_EXT:
        return True  # Using Blender's image/movie filter.
    ext = os.path.splitext(fn)[1].lstrip(".").lower()
    return ext in EXT_FILTER[ext_key][0]


# -----------------------------------------------------------------------------
# Cycles utils.
def get_input_nodes(node, nodes, links):
    # Get all links going to node.
    input_links = {lnk for lnk in links if lnk.to_node == node}
    # Sort those links, get their input nodes (and avoid doubles!).
    sorted_nodes = []
    done_nodes = set()
    for socket in node.inputs:
        done_links = set()
        for link in input_links:
            nd = link.from_node
            if nd in done_nodes:
                # Node already treated!
                done_links.add(link)
            elif link.to_socket == socket:
                sorted_nodes.append(nd)
                done_links.add(link)
                done_nodes.add(nd)
        input_links -= done_links
    return sorted_nodes

#_________________________________________________________
def auto_align_nodes(node_tree):
    print('\nAligning Nodes')
    x_gap = 200
    y_gap = 100
    nodes = node_tree.nodes
    links = node_tree.links
    to_node = None
    for node in nodes:
        if node.type == 'OUTPUT_MATERIAL':
            to_node = node
            break
    if not to_node:
        return  # Unlikely, but bette check anyway...
    #_________________________________________________________
    def align(to_node, nodes, links):
        from_nodes = get_input_nodes(to_node, nodes, links)
        for i, node in enumerate(from_nodes):
            node.location.x = to_node.location.x - x_gap
            node.location.y = to_node.location.y
            node.location.y -= i * y_gap
            node.location.y += (len(from_nodes)-1) * y_gap / (len(from_nodes))
            align(node, nodes, links)

    align(to_node, nodes, links)

#_________________________________________________________

def clean_node_tree(node_tree):
    nodes = node_tree.nodes
    for node in nodes:
        if not node.type == 'OUTPUT_MATERIAL':
            nodes.remove(node)
    return node_tree.nodes[0]


# -----------------------------------------------------------------------------
# Operator

class IMPORT_OT_image_to_plane(Operator, AddObjectHelper):
    """Create mesh plane(s) from image files with the appropiate aspect ratio"""
    bl_space_type = 'VIEW_3D'
    bl_region_type = 'TOOLS'
    bl_category = 'BLEN2D'
    bl_idname = "import_image.to_plane"
    bl_label = "Import HTML5 Assets"
    bl_options = {'REGISTER', 'UNDO'}

    # -----------
    # File props.
    files = CollectionProperty(type=bpy.types.OperatorFileListElement, options={'HIDDEN', 'SKIP_SAVE'})

    directory = StringProperty(maxlen=1024, subtype='FILE_PATH', options={'HIDDEN', 'SKIP_SAVE'})

    # Show only images/videos, and directories!
    filter_image = BoolProperty(default=True, options={'HIDDEN', 'SKIP_SAVE'})
    filter_movie = BoolProperty(default=True, options={'HIDDEN', 'SKIP_SAVE'})
    filter_folder = BoolProperty(default=True, options={'HIDDEN', 'SKIP_SAVE'})
    filter_glob = StringProperty(default="", options={'HIDDEN', 'SKIP_SAVE'})

    # --------
    # Options.
    align = BoolProperty(name="Align Planes", default=True, description="Create Planes in a row")

    align_offset = FloatProperty(name="Offset", min=0, soft_min=0, default=0.1, description="Space between Planes")

    # Callback which will update File window's filter options accordingly to extension setting.
    def update_extensions(self, context):
        if self.extension == DEFAULT_EXT:
            self.filter_image = True
            self.filter_movie = True
            self.filter_glob = ""
        else:
            self.filter_image = False
            self.filter_movie = False
            flt = ";".join(("*." + e for e in EXT_FILTER[self.extension][0]))
            self.filter_glob = flt
        # And now update space (file select window), if possible.
        space = bpy.context.space_data
        # XXX Can't use direct op comparison, these are not the same objects!
        if (space.type != 'FILE_BROWSER' or space.operator.bl_rna.identifier != self.bl_rna.identifier):
            return
        space.params.use_filter_image = self.filter_image
        space.params.use_filter_movie = self.filter_movie
        space.params.filter_glob = self.filter_glob
        # XXX Seems to be necessary, else not all changes above take effect...
        #~ bpy.ops.file.refresh()
    extension = EnumProperty(name="Extension", items=gen_ext_filter_ui_items(),
                             description="Only import files of this type", update=update_extensions)

    # -------------------
    # Plane size options.
    _size_modes = (
        ('ABSOLUTE', "Absolute", "Use absolute size"),
        ('DPI', "Dpi", "Use definition of the image as dots per inch"),
        ('DPBU', "Dots/BU", "Use definition of the image as dots per Blender Unit"),
    )
    size_mode = EnumProperty(name="Size Mode", default='DPBU', items=_size_modes,
                             description="How the size of the plane is computed")

    height = FloatProperty(name="Height", description="Height of the created plane",
                           default=1.0, min=0.001, soft_min=0.001, subtype='DISTANCE', unit='LENGTH')

    factor = FloatProperty(name="Definition", min=1.0, default=100.0,
                           description="Number of pixels per inch or Blender Unit")

    # -------------------------
    # Blender material options.
    t = bpy.types.Material.bl_rna.properties["use_shadeless"]
    use_shadeless = BoolProperty(name=t.name, default=True, description=t.description)#@me:o defalut = False

    use_transparency = BoolProperty(name="Use Alpha", default=True, description="Use alphachannel for transparency")#@me:o defalut = False

    t = bpy.types.Material.bl_rna.properties["transparency_method"]
    items = tuple((it.identifier, it.name, it.description) for it in t.enum_items)
    transparency_method = EnumProperty(name="Transp. Method", description=t.description, items=items)

    t = bpy.types.Material.bl_rna.properties["use_transparent_shadows"]
    use_transparent_shadows = BoolProperty(name=t.name, default=False, description=t.description)

    #-------------------------
    # Cycles material options.
    shader = EnumProperty(name="Shader", items=CYCLES_SHADERS, description="Node shader to use")

    overwrite_node_tree = BoolProperty(name="Overwrite Material", default=True,
                                       description="Overwrite existing Material with new nodetree "
                                                   "(based on material name)")

    # --------------
    # Image Options.
    t = bpy.types.Image.bl_rna.properties["alpha_mode"]
    alpha_mode_items = tuple((e.identifier, e.name, e.description) for e in t.enum_items)
    alpha_mode = EnumProperty(name=t.name, items=alpha_mode_items, default=t.default, description=t.description)

    t = bpy.types.IMAGE_OT_match_movie_length.bl_rna
    match_len = BoolProperty(name=t.name, default=True, description=t.description)

    t = bpy.types.Image.bl_rna.properties["use_fields"]
    use_fields = BoolProperty(name=t.name, default=False, description=t.description)

    t = bpy.types.ImageUser.bl_rna.properties["use_auto_refresh"]
    use_auto_refresh = BoolProperty(name=t.name, default=False, description=t.description)#@me:o defalut = True

    relative = BoolProperty(name="Relative", default=True, description="Apply relative paths")

    def draw(self, context):
        engine = context.scene.render.engine
        layout = self.layout

        box = layout.box()
        box.label("Import Options:", icon='FILTER')
        box.prop(self, "extension", icon='FILE_IMAGE')
        box.prop(self, "align")
        box.prop(self, "align_offset")

        row = box.row()
        row.active = bpy.data.is_saved
        row.prop(self, "relative")
        box.prop(self, "match_len")
        row = box.row()
        row.prop(self, "use_transparency")
        sub = row.row()
        sub.active = self.use_transparency
        sub.prop(self, "alpha_mode", text="")
        box.prop(self, "use_fields")
        box.prop(self, "use_auto_refresh")

        box = layout.box()
        if engine == 'BLENDER_RENDER':
            box.label("Material Settings: (Blender)", icon='MATERIAL')
            box.prop(self, "use_shadeless")
            row = box.row()
            row.prop(self, "transparency_method", expand=True)
            box.prop(self, "use_transparent_shadows")
        elif engine == 'CYCLES':
            box = layout.box()
            box.label("Material Settings: (Cycles)", icon='MATERIAL')
            box.prop(self, 'shader', expand = True)
            box.prop(self, 'overwrite_node_tree')

        box = layout.box()
        box.label("Plane dimensions:", icon='ARROW_LEFTRIGHT')
        row = box.row()
        row.prop(self, "size_mode", expand=True)
        if self.size_mode == 'ABSOLUTE':
            box.prop(self, "height")
        else:
            box.prop(self, "factor")

    def invoke(self, context, event):
        self.update_extensions(context)
        context.window_manager.fileselect_add(self)
        return {'RUNNING_MODAL'}

    def execute(self, context):
        if not bpy.data.is_saved:
            self.relative = False

        # the add utils don't work in this case because many objects are added disable relevant things beforehand
        editmode = context.user_preferences.edit.use_enter_edit_mode
        context.user_preferences.edit.use_enter_edit_mode = False
        if context.active_object and context.active_object.mode == 'EDIT':
            bpy.ops.object.mode_set(mode='OBJECT')

        self.import_images(context)

        context.user_preferences.edit.use_enter_edit_mode = editmode
        return {'FINISHED'}

    # Main...
    def import_images(self, context):
        engine = context.scene.render.engine
        import_list, directory = self.generate_paths()

        images = tuple(load_image(path, directory) for path in import_list)

        for img in images:
            self.set_image_options(img)

        if engine in {'BLENDER_RENDER', 'BLENDER_GAME'}:
            textures = (self.create_image_textures(context, img) for img in images)
            materials = (self.create_material_for_texture(tex) for tex in textures)
        elif engine == 'CYCLES':
            materials = (self.create_cycles_material(context, img) for img in images)
        else:
            return

        planes = tuple(self.create_image_plane(context, mat) for mat in materials)

        context.scene.update()
        if self.align:
            self.align_planes(planes)

        for plane in planes:
            plane.select = True

        self.report({'INFO'}, "Added {} Image Plane(s)".format(len(planes)))

    def set_child_to_HTML5_root(self, child):
      objects = bpy.data.objects;
      parent = objects.get('@HTML5 root')
      if(parent):
        child.parent = parent
      else:
        bpy.ops.object.empty_add()
        empty = bpy.context.object
        empty.name = '@HTML5 root'
        child.parent = empty
        empty.hide = True
        empty.hide_select = True
        empty.hide_render = True

        bpy.ops.object.empty_add()
        br = bpy.context.object
        br.name = '~--------------------------------------------------------------------------------'
        br.parent = empty
        br.hide = True
        br.hide_select = True
        br.hide_render = True


    def add_to_HTML5_assets_library(self, plane):
      lastLayer = (False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,True)
      objects = bpy.data.objects;
      parent = objects.get('@Assets Library')
      if(parent):
        plane.parent = parent
      else:
        # bpy.ops.object.empty_add(type='PLAIN_AXES', radius=1, view_align=False, location=(0, 0, 0), rotation=(0, 0, 0), layers=(False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, True))
        bpy.ops.object.empty_add(layers = lastLayer)
        empty = bpy.context.object
        empty.name = '@Assets Library'
        plane.parent = empty
        empty.hide = True
        empty.hide_select = True
        empty.hide_render = True

        bpy.ops.object.empty_add()
        br = bpy.context.object
        br.name = '~--------------------------------------------------------------------------'
        br.parent = empty
        br.hide = True
        br.hide_select = True
        br.hide_render = True

      plane.hide = True
      plane.layers = lastLayer
      plane.html5type = 'MESH'
      plane.lock_rotation[0] = True
      plane.lock_rotation[1] = True
      plane.lock_scale[2] = True

    def create_image_plane(self, context, material):
        engine = context.scene.render.engine
        if engine in {'BLENDER_RENDER', 'BLENDER_GAME'}:
            img = material.texture_slots[0].texture.image
        elif engine == 'CYCLES':
            nodes = material.node_tree.nodes
            img = next((node.image for node in nodes if node.type == 'TEX_IMAGE'))
        px, py = img.size

        # can't load data
        if px == 0 or py == 0:
            px = py = 1

        if self.size_mode == 'ABSOLUTE':
            y = self.height
            x = px / py * y
        elif self.size_mode == 'DPI':
            fact = 1 / self.factor / context.scene.unit_settings.scale_length * 0.0254
            x = px * fact
            y = py * fact
        else:  # elif self.size_mode == 'DPBU'
            fact = 1 / self.factor
            x = px * fact
            y = py * fact

        bpy.ops.mesh.primitive_plane_add('INVOKE_REGION_WIN')
        plane = context.scene.objects.active
        # Why does mesh.primitive_plane_add leave the object in edit mode???
        if plane.mode is not 'OBJECT':
            bpy.ops.object.mode_set(mode='OBJECT')
        plane.dimensions = x, y, 0.0
        plane.name = material.name
        bpy.ops.object.transform_apply(scale=True)
        plane.data.uv_textures.new()
        plane.data.materials.append(material)
        plane.data.uv_textures[0].data[0].image = img

        material.game_settings.use_backface_culling = False
        material.game_settings.alpha_blend = 'ALPHA'

        self.add_to_HTML5_assets_library(plane)
        return plane

    def align_planes(self, planes):
        gap = self.align_offset
        offset = 0
        for i, plane in enumerate(planes):
            offset += (plane.dimensions.x / 2.0) + gap
            if i == 0:
                continue
            move_local = mathutils.Vector((offset, 0.0, 0.0))
            move_world = plane.location + move_local * plane.matrix_world.inverted()
            plane.location += move_world
            offset += (plane.dimensions.x / 2.0)

    def generate_paths(self):
        return (fn.name for fn in self.files if is_image_fn(fn.name, self.extension)), self.directory

    # Internal
    def create_image_textures(self, context, image):
        fn_full = os.path.normpath(bpy.path.abspath(image.filepath))

        # look for texture with importsettings
        for texture in bpy.data.textures:
            if texture.type == 'IMAGE':
                tex_img = texture.image
                if (tex_img is not None) and (tex_img.library is None):
                    fn_tex_full = os.path.normpath(bpy.path.abspath(tex_img.filepath))
                    if fn_full == fn_tex_full:
                        self.set_texture_options(context, texture)
                        return texture

        # if no texture is found: create one
        name_compat = bpy.path.display_name_from_filepath(image.filepath)
        texture = bpy.data.textures.new(name=name_compat, type='IMAGE')
        texture.image = image
        self.set_texture_options(context, texture)
        return texture

    def create_material_for_texture(self, texture):
        # look for material with the needed texture
        for material in bpy.data.materials:
            slot = material.texture_slots[0]
            if slot and slot.texture == texture:
                self.set_material_options(material, slot)
                return material

        # if no material found: create one
        name_compat = bpy.path.display_name_from_filepath(texture.image.filepath)
        material = bpy.data.materials.new(name=name_compat)
        slot = material.texture_slots.add()
        slot.texture = texture
        slot.texture_coords = 'UV'
        self.set_material_options(material, slot)
        return material

    def set_image_options(self, image):
        image.use_alpha = self.use_transparency
        image.alpha_mode = self.alpha_mode
        image.use_fields = self.use_fields

        if self.relative:
            try:  # can't always find the relative path (between drive letters on windows)
                image.filepath = bpy.path.relpath(image.filepath)
            except ValueError:
                pass

    def set_texture_options(self, context, texture):
        texture.image_user.use_auto_refresh = self.use_auto_refresh
        if self.match_len:
            texture.image_user.frame_duration = texture.image.frame_duration

    def set_material_options(self, material, slot):
        if self.use_transparency:
            material.alpha = 0.0
            material.specular_alpha = 0.0
            slot.use_map_alpha = True
        else:
            material.alpha = 1.0
            material.specular_alpha = 1.0
            slot.use_map_alpha = False
        material.use_transparency = self.use_transparency
        material.transparency_method = self.transparency_method
        material.use_shadeless = self.use_shadeless
        material.use_transparent_shadows = self.use_transparent_shadows

    #--------------------------------------------------------------------------
    # Cycles
    def create_cycles_texnode(self, context, node_tree, image):
        tex_image = node_tree.nodes.new('ShaderNodeTexImage')
        tex_image.image = image
        tex_image.show_texture = True
        self.set_texture_options(context, tex_image)
        return tex_image

    def create_cycles_material(self, context, image):
        name_compat = bpy.path.display_name_from_filepath(image.filepath)
        material = None
        for mat in bpy.data.materials:
            if mat.name == name_compat and self.overwrite_node_tree:
                material = mat
        if not material:
            material = bpy.data.materials.new(name=name_compat)

        material.use_nodes = True
        node_tree = material.node_tree
        out_node = clean_node_tree(node_tree)

        tex_image = self.create_cycles_texnode(context, node_tree, image)

        if self.shader == 'BSDF_DIFFUSE':
            bsdf_diffuse = node_tree.nodes.new('ShaderNodeBsdfDiffuse')
            node_tree.links.new(out_node.inputs[0], bsdf_diffuse.outputs[0])
            node_tree.links.new(bsdf_diffuse.inputs[0], tex_image.outputs[0])

        elif self.shader == 'EMISSION':
            emission = node_tree.nodes.new('ShaderNodeEmission')
            lightpath = node_tree.nodes.new('ShaderNodeLightPath')
            node_tree.links.new(out_node.inputs[0], emission.outputs[0])
            node_tree.links.new(emission.inputs[0], tex_image.outputs[0])
            node_tree.links.new(emission.inputs[1], lightpath.outputs[0])

        elif self.shader == 'BSDF_DIFFUSE_BSDF_TRANSPARENT':
            bsdf_diffuse = node_tree.nodes.new('ShaderNodeBsdfDiffuse')
            bsdf_transparent = node_tree.nodes.new('ShaderNodeBsdfTransparent')
            mix_shader = node_tree.nodes.new('ShaderNodeMixShader')
            node_tree.links.new(out_node.inputs[0], mix_shader.outputs[0])
            node_tree.links.new(mix_shader.inputs[0], tex_image.outputs[1])
            node_tree.links.new(mix_shader.inputs[2], bsdf_diffuse.outputs[0])
            node_tree.links.new(mix_shader.inputs[1], bsdf_transparent.outputs[0])
            node_tree.links.new(bsdf_diffuse.inputs[0], tex_image.outputs[0])

        elif self.shader == 'EMISSION_BSDF_TRANSPARENT':
            emission = node_tree.nodes.new('ShaderNodeEmission')
            lightpath = node_tree.nodes.new('ShaderNodeLightPath')
            bsdf_transparent = node_tree.nodes.new('ShaderNodeBsdfTransparent')
            mix_shader = node_tree.nodes.new('ShaderNodeMixShader')
            node_tree.links.new(out_node.inputs[0], mix_shader.outputs[0])
            node_tree.links.new(mix_shader.inputs[0], tex_image.outputs[1])
            node_tree.links.new(mix_shader.inputs[2], emission.outputs[0])
            node_tree.links.new(mix_shader.inputs[1], bsdf_transparent.outputs[0])
            node_tree.links.new(emission.inputs[0], tex_image.outputs[0])
            node_tree.links.new(emission.inputs[1], lightpath.outputs[0])

        auto_align_nodes(node_tree)
        return material

# -----------------------------------------------------------------------------

# Register
def import_images_button(self, context):
    self.layout.operator(IMPORT_OT_image_to_plane.bl_idname,
                         text="Import HTML5 Assets", icon='TEXTURE')
def export_blen2d_button(self, context):
    self.layout.operator(ExportBlen2d.bl_idname,
                         text="Export Scene to JS", icon='TEXTURE')

# ******************************************************************************************
# ******************************************************************************************

# panel containing all tools
class HTML5_Animation_Shape_Key(Panel):
    bl_space_type = 'VIEW_3D'
    bl_region_type = 'TOOLS'
    bl_category = 'BLEN2D'
    #bl_context = 'objectmode'
    bl_label = 'HTML5 Shape Keys'
    bl_options = {'DEFAULT_CLOSED'}
      
    COMPAT_ENGINES = {'BLENDER_RENDER'}

    @classmethod
    def poll(cls, context):
        engine = context.scene.render.engine
        obj = context.object
        return (obj and obj.type in {'MESH', 'LATTICE', 'CURVE', 'SURFACE'} and (engine in cls.COMPAT_ENGINES))

    def draw(self, context):
        layout = self.layout
        ob = context.object
        key = ob.data.shape_keys
        kb = ob.active_shape_key

        enable_edit = ob.mode != 'EDIT'
        enable_edit_value = False

        if ob.show_only_shape_key is False:
            if enable_edit or (ob.type == 'MESH' and ob.use_shape_key_edit_mode):        enable_edit_value = True

        row = layout.row()

        rows = 2
        if kb:
          rows = 4
        row.template_list("MESH_UL_shape_keys", "", key, "key_blocks", ob, "active_shape_key_index", rows=rows)

        col = row.column()

        sub = col.column(align=True)
        sub.operator("object.shape_key_add", icon='ZOOMIN', text="").from_mix = False
        sub.operator("object.shape_key_remove", icon='ZOOMOUT', text="").all = False
        sub.menu("MESH_MT_shape_key_specials", icon='DOWNARROW_HLT', text="")

        if kb:
            col.separator()

            sub = col.column(align=True)
            sub.operator("object.shape_key_move", icon='TRIA_UP', text="").type = 'UP'
            sub.operator("object.shape_key_move", icon='TRIA_DOWN', text="").type = 'DOWN'

            split = layout.split(percentage=0.4)
            row = split.row()
            row.enabled = enable_edit
            row.prop(key, "use_relative")

            row = split.row()
            row.alignment = 'RIGHT'

            sub = row.row(align=True)
            sub.label()  # XXX, for alignment only
            subsub = sub.row(align=True)
            subsub.active = enable_edit_value
            subsub.prop(ob, "show_only_shape_key", text="")
            sub.prop(ob, "use_shape_key_edit_mode", text="")

            sub = row.row()
            if key.use_relative:
                sub.operator("object.shape_key_clear", icon='X', text="")
            else:
                sub.operator("object.shape_key_retime", icon='RECOVER_LAST', text="")

            if key.use_relative:
                if ob.active_shape_key_index != 0:
                    row = layout.row()
                    row.active = enable_edit_value
                    row.prop(kb, "value")

                    split = layout.split()

                    col = split.column(align=True)
                    col.active = enable_edit_value
                    col.label(text="Range:")
                    col.prop(kb, "slider_min", text="Min")
                    col.prop(kb, "slider_max", text="Max")

                col = split.column(align=True)
                col.active = enable_edit_value
                col.label(text="Blend:")
                col.prop_search(kb, "vertex_group", ob, "vertex_groups", text="")
                col.prop_search(kb, "relative_key", key, "key_blocks", text="")

            else:
                layout.prop(kb, "interpolation")
                row = layout.column()
                row.active = enable_edit_value
                row.prop(key, "eval_time")

# -----------------------------------------------------------------------------

class HTML5_Animation_Tool(Panel):
    bl_space_type = 'VIEW_3D'
    bl_region_type = 'TOOLS'
    bl_category = "BLEN2D"
    bl_label = "HTML5 Assets"

    def draw(self, context):
        layout = self.layout
        col = layout.column(align=True)
        obj = context.object
        col.prop(obj, "html5type", expand=False)
        col = layout.column(align=True)
        col.operator(IMPORT_OT_image_to_plane.bl_idname, text="Import Assets", icon='MESH_PLANE')
        col = layout.column(align=True)
        col.operator(ExportBlen2d.bl_idname, text="Expot to JS file", icon='OUTLINER_OB_FONT')

# ******************************************************************************************
# ******************************************************************************************
class Animation2D_Add_New(Operator):
    """Create a New 2D Animation"""
    bl_idname = "blend2d.create_new_animation"
    bl_label = "Create new 2D Animation"

    def execute(self, context):
        obj = context.object
        sceneanim = None
        if len(context.scene.animation2D_list):
            sceneanim = context.scene.animation2D_list[0]
        else:
            context.scene.animation2D_list.add()
            sceneanim = context.scene.animation2D_list[0]

        if (obj.name[0] is not '@') and (obj.name[0] is not '~'):
            anim = obj.animation2D_list.add()
            anim.index = sceneanim.private_index
            anim.name = anim.name+"_"+str(anim.index)
            sceneanim.private_index+=1
            obj.animation2D_listINDEX = len(obj.animation2D_list)-1
            sceneanim.obj[str(anim.index)] = []
            sceneanim.act[str(anim.index)] = []
            # sceneanim.animation[str(anim.index)] = [anim] # storing it to name
            self.create_anim(context, obj, anim)

        else:
            return{'CANCELLED'}


        return {'FINISHED'}

    def create_anim(self,context, obj, anim):
        sceneanim = context.scene.animation2D_list[0]
        if obj.animation_data and obj.animation_data.action:
            sceneanim.obj[str(anim.index)].append(obj)
            sceneanim.act[str(anim.index)].append(obj.animation_data.action)
            

        for o in obj.children:
            self.create_anim(context, o, anim)

# -----------------------------------------------------------------------------

class Animation2D_Remove(Operator):
    """Remove 2D Animation"""
    bl_idname = "blend2d.remove_animation"
    bl_label = "Remove 2D Animation"

    @classmethod
    def poll(self, context):
        """ Enable if there's something in the list """
        return len(context.object.animation2D_list) > 0


    def execute(self, context):
        anim_list = context.object.animation2D_list
        anim_index = context.object.animation2D_listINDEX

        sceneanim = context.scene.animation2D_list[0]

        skey = str(anim_list[anim_index].index)
        if sceneanim.obj.get(skey,None):
            sceneanim.obj.pop(skey)
        if sceneanim.act.get(skey,None):
            sceneanim.act.pop(skey)
        # sceneanim.animation.pop(str(anim_list[anim_index].index))

        anim_list.remove(anim_index)

        if anim_index > 0:
            context.object.animation2D_listINDEX-=1
        return {'FINISHED'}

# -----------------------------------------------------------------------------

class LoadThisAnimation(Operator):
    """Load selected Animation to scene"""
    bl_idname = "blend2d.load_animation"
    bl_label = "Load 2D Animation to scene"

    @classmethod
    def poll(self, context):
        """ Enable if there's something in the list """
        return len(context.object.animation2D_list) > 0


    def execute(self, context):
        anim_list = context.object.animation2D_list
        anim_index = context.object.animation2D_listINDEX

        sceneanim = context.scene.animation2D_list[0]

        try:
            obj = sceneanim.obj[str(anim_list[anim_index].index)]# obj is list of objects
            act = sceneanim.act[str(anim_list[anim_index].index)]# act is list of actions

            self.load_None_animation(context.object)
            if len(obj) == len(act):
                for i in range(len(obj)):
                    if obj[i] and act[i]:#either of them could be none
                        if obj[i].animation_data:
                            if act[i]:
                                obj[i].animation_data.action = act[i]

                
            else:
                print("Error in " + self.bl_idname)
        except KeyError:
            print("Error in finding key "+ str(anim_list[anim_index].index))
            

        return {'FINISHED'}


    def load_None_animation(self, obj):
        if obj.animation_data:
            if obj.animation_data.action:
                obj.animation_data.action = None
        for o in obj.children:
            self.load_None_animation(o)

# -----------------------------------------------------------------------------

class LoadAnimation2DdataAfterAddonToggle():
    """If this addon is removed and reinitiated some data are lost
        as they are not part of the bpy properties. They have to be initialised again for the Addon to work
    """
    bl_idname = "blend2d.reinitiate.animdataonaddontoggle"
    bl_label = "Re initiate animation2D data"

    def execute(self, context):
        load_animation_data_from_string(context.scene)
        return {'FINISHED'}

# ******************************************************************************************
# ******************************************************************************************

class Blen2D_Local_Transform_Property_Panel(Panel):
    bl_space_type = 'VIEW_3D'
    bl_region_type = 'TOOLS'
    bl_category = "BLEN2D"
    bl_label = "Transform"
    #_________________________________________________________
    def clear_inverse_parent_matrix(obj):
        if obj.name[0] in '@~':
            pass
        else:
            identity = mathutils.Matrix()
            if obj.matrix_parent_inverse != identity:
                obj.matrix_parent_inverse = identity

        for c in obj.children:
            Blen2D_Local_Transform_Property_Panel.clear_inverse_parent_matrix(c)

    #_________________________________________________________

    def update_local_transform():
        obj = bpy.context.object
        mat = obj.matrix_local.copy()
        obj.local_location = mat.to_translation()
        obj.local_rotation = mat.to_euler()
        obj.local_rotation[0] = obj.local_rotation[0] *(180/math.pi)
        obj.local_rotation[1] = obj.local_rotation[1] *(180/math.pi)
        obj.local_rotation[2] = obj.local_rotation[2] *(180/math.pi)
        obj.local_scale = mat.to_scale()
    #_________________________________________________________

    class ClearMatrixParentInverse(Operator):
        """Clears parent inverse matrix"""
        bl_idname = "blend2d.clear_parent_inverse_matrix"
        bl_label = "Clear Parent Inverse Matrix"

        def execute(self, context):
            root=bpy.data.objects.get('@HTML5 root',None)
            if root:
                Blen2D_Local_Transform_Property_Panel.clear_inverse_parent_matrix(root)
            return {'FINISHED'}

    #_________________________________________________________

    class UpdateLocalTranformation(Operator):
        """Update Local transforms"""
        bl_idname = "blend2d.update_local_transform"
        bl_label = "Update"

        @classmethod
        def poll(self, context):
            """ Enable if """
            if context.object:
                return True
            else:
                return False

        def execute(self, context):
            Blen2D_Local_Transform_Property_Panel.update_local_transform()
            return {'FINISHED'}
    #_________________________________________________________

    def draw(self, context):
        layout = self.layout
        if context.object:
            ob = context.object
            row = layout.row()
            row.operator(self.ClearMatrixParentInverse.bl_idname, text="Clear PI matrix for All", icon='CANCEL')
            row = layout.row()
            row.operator(self.UpdateLocalTranformation.bl_idname, text="Update Transform", icon='MESH_PLANE')

            row = layout.row()
            row.column().prop(ob, "local_location")
            # row.column().prop(ob, "rotation_euler")
            row.column().prop(ob, "local_rotation")
            row.column().prop(ob, "local_scale")

            row = layout.row()
            row.column().prop(ob, "dimensions")
        
# -----------------------------------------------------------------------------

class Animation2DPanel(Panel):
    bl_space_type = 'VIEW_3D'
    bl_region_type = 'TOOLS'
    bl_category = "BLEN2D"
    bl_label = "2D Animations"
    #_________________________________________________________

    def draw(self, context):
        layout = self.layout
        col = layout.column(align=True)
        if context.object:
            if context.object.animation_data:
                col.template_ID(context.object.animation_data, "action", new="action.new")

            obj = context.object
            box = layout.box()
            col = box.column(align=True)
            col.operator(Animation2D_Add_New.bl_idname, text="Create 2D Animation", icon='RENDER_ANIMATION')

            # if len(context.scene.animation2D_list):
            #     if len(context.scene.animation2Dsaveddata) - len(context.scene.animation2D_list[0].obj):
            #         col = box.column()
            #         col.operator(LoadAnimation2DdataAfterAddonToggle.bl_idname, text="Load missing data", icon='PASTEDOWN')


            
            row = box.row(align=True)
            if obj.animation2D_listINDEX >= 0 and len(obj.animation2D_list) > 0:
                item = obj.animation2D_list[obj.animation2D_listINDEX]
                row.prop(item, "name")
                # row.prop(obj.animation_data.nla_tracks[0], "name")
                row.operator(Animation2D_Remove.bl_idname, text="", icon='X')
            row = box.row()
            row.template_list("Animation2D_UI_List", "The_List", obj, "animation2D_list", obj, "animation2D_listINDEX" )
            col = box.column(align=True)
            col.operator(LoadThisAnimation.bl_idname, text="Load this Animation", icon='NLA_PUSHDOWN')

# ******************************************************************************************
# ******************************************************************************************

class ExportBlen2d(Operator, ExportHelper):
    """Export scene data to a JS file """
    bl_idname = "export.blen2d"
    bl_label = "Export BLEN2D (.js)"

    filename_ext = ".js"
    filter_glob = StringProperty(default="*.js", options={'HIDDEN'})

    # y_up = BoolProperty(
    #         name="Make Y up",
    #         description="Switch the Y and Z axis",
    #         default=True)
    # rot_ord = rotation_order
    filedatadict = OrderedDict()
    out_source_dir = None
    out_dest_dir = None
    def execute(self, context):
        return self.write_blen2d(context, self.filepath)

    #_________________________________________________________

    def get_blend2d_dict(self, obj, types):
        ldict = OrderedDict()
        # bpy.ops.wm.properties_add(data_path="object")
    
        if types == 'assets':
            ldict['name'] = obj.name
            ldict['type'] = obj.html5type
            if obj.type == 'MESH':
                ldict['width'] = PPI*obj.dimensions.x
                ldict['height'] = PPI*obj.dimensions.y
                print('working')
                if len(obj.material_slots):
                    img = obj.material_slots[0].material.active_texture.image
                    # string = path_reference(img.filepath, self.out_source_dir, self.out_dest_dir,
                                                              # 'RELATIVE', "", set(), img.library)
                    string = path_reference(img.filepath, self.out_source_dir, self.out_source_dir,
                                                              'RELATIVE', "", set(), img.library)
                    ldict['texturepath'] = string.replace('\\','/')

                    slist = img.filepath.split('\\')
                    texname = slist[len(slist)-1]
                    ldict['texture'] = texname
                
      
        elif types == 'objects':
            ldict['name'] = obj.name
            ldict['type'] = obj.html5type


            #Local#working
            # mat = obj.matrix_local.copy()
            # ldict['x'] = round((PPI*mat.to_translation().x)*PPI*10)/(PPI*10)
            # ldict['y'] = -round((PPI*mat.to_translation().y)*PPI*10)/(PPI*10)
            # ldict['zindex'] = round((PPI*mat.to_translation().z)*PPI*10)/(PPI*10)
            # ldict['rotation'] = -mat.to_euler().z*(180/math.pi)
            # ldict['scalex'] = mat.to_scale().x
            # ldict['scaley'] =  mat.to_scale().y


            # mat = obj.matrix_local.copy()
            # ldict['x'] = PPI*mat.to_translation().x
            # ldict['y'] = -PPI*mat.to_translation().y
            # ldict['zindex'] = PPI*mat.to_translation().z
            # ldict['rotation'] = -mat.to_euler().z
            # ldict['scalex'] = mat.to_scale().x
            # ldict['scaley'] =  mat.to_scale().y

            #Global
            ldict['x'] = PPI*obj.location.x
            ldict['y'] = -PPI*obj.location.y
            ldict['zindex'] = PPI*obj.location.z
            ldict['rotation'] = -obj.rotation_euler.z
            ldict['scalex'] = obj.scale.x
            ldict['scaley'] = obj.scale.y




            if obj.type == 'MESH':
                try:
                    me = obj.to_mesh(bpy.context.scene, False, 'PREVIEW', calc_tessface=False)
                except RuntimeError:
                    me = None

                if me is None:
                    anchorx = 0
                    anchory = 0
                else:
                    xlist = [me.vertices[i].co.x for i in range(len(me.vertices))]
                    xmin = min(xlist)
                    xmax = max(xlist)
                    width = xmax - xmin or 1
                    anchorx = -xmin/width 
                    ylist = [me.vertices[i].co.y for i in range(len(me.vertices))]
                    ymin = min(ylist)
                    ymax = max(ylist)
                    height = ymax - ymin or 1
                    anchory = ymax/height

                ldict['anchorx'] = anchorx
                ldict['anchory'] = anchory

                if len(obj.material_slots):
                    slist = obj.material_slots[0].material.active_texture.image.filepath.split('\\')
                    texname = slist[len(slist)-1]
                    ldict['texture'] = texname
                else:
                    ldict['texture'] = 0
                if obj.animation_data:
                    ldict['action'] = obj.animation_data.action.name
                else:
                    ldict['action'] = 0

                
            elif obj.type =='EMPTY':
                pass
            animsdict = self.get_blend2d_animations_dict(obj)
            ldict['animations'] = animsdict
            ldict['animationsCount'] = len(animsdict)
            actsdict = self.get_blend2d_actions_dict(obj)
            ldict['actions'] = actsdict
            ldict['actionsCount'] = len(actsdict)

        elif types == 'props':
            ldict['name'] = obj.name
            if obj.name == 'game boundary':
                ldict['gameWidth'] = PPI*round(obj.dimensions.x * PPI*10)/(PPI*10)
                ldict['gameHeight'] = PPI*round(obj.dimensions.y * PPI*10)/(PPI*10)

        


        validchildren = []
        for o in obj.children:
            if o.name[0] is not '~':
                validchildren.append(o)

        contentlist = [self.get_blend2d_dict(o,types) for o in validchildren]
        if types == 'objects':
            ldict['contents'] = sorted(contentlist, key=itemgetter('zindex'), reverse=False)
        else:
            ldict['contents'] = contentlist

        return ldict
    #_________________________________________________________

    def get_blend2d_animations_dict(self, obj):#poll when it has animation
        ldict = OrderedDict()
        scene = bpy.context.scene
        if len(scene.animation2D_list):
            animscene = scene.animation2D_list[0]
            if len(obj.animation2D_list):
                for animobj in obj.animation2D_list:
                    oneanimdict = OrderedDict()
                    objlist = animscene.obj[str(animobj.index)]
                    actlist = animscene.act[str(animobj.index)]

                    # **********************************************
                    # to find out min and max frame of this animation
                    minFrame = 90000000000
                    maxFrame = 0
                    for act in actlist:
                        if act.frame_range[0] < minFrame:
                            minFrame = act.frame_range[0]
                        if act.frame_range[1] > maxFrame:
                            maxFrame = act.frame_range[1]
                    if maxFrame <= minFrame:
                        minFrame = 0
                        maxFrame = 0

                    oneanimdict['minmaxFrame'] = [minFrame, maxFrame]

                    # **********************************************
                    oneanimdict['objactbinding'] = OrderedDict()
                    if len(objlist) == len(actlist):
                        for i in range(len(objlist)):
                            if actlist[i] and objlist[i]:#could be None type
                                oneanimdict['objactbinding'][objlist[i].name] = actlist[i].name
                    else:
                        print("error in get_blend2d_animation_dict 1")
                    ldict[animobj.name] = oneanimdict
            else:
                print(obj.name+" has no animation")

        return ldict
    #_________________________________________________________

    def get_blend2d_actions_dict(self, obj):
        ldict = OrderedDict()
        scene = bpy.context.scene
        if len(scene.animation2D_list):
            animscene = scene.animation2D_list[0]
            for k in animscene.obj:
                olist = animscene.obj[k]
                alist = animscene.act[k]
                for i in range(len(olist)):
                    if olist[i].name == obj.name:
                        if alist[i]:
                            action = alist[i]
                            ldict[action.name] = self.get_fcurve_keyframes_list(action)
                            # ldict[action.name] = self.get_fcurve_local_keyframes_list(obj, action)

        else:
            print("scene has no animation")

        return ldict
    #_________________________________________________________

    def get_fcurve_keyframes_list(self, action):
        keyframes = OrderedDict()
        for curve in action.fcurves:
            axis = curve.array_index
            if curve.data_path == 'location':
                if axis == 0:#X
                    framelist = []
                    for point in curve.keyframe_points:
                        framelist.append({'frame': point.co[0], 'value': PPI*point.co[1]})
                    keyframes['x'] = OrderedDict()
                    # keyframes['x']['fromKeyframeIndex'] = 0
                    # keyframes['x']['fromKeyframeIndexSaved'] = 0
                    keyframes['x']['datalist'] = sorted(framelist, key=itemgetter('frame'), reverse=False)
                elif axis == 1:#Y
                    framelist = []
                    for point in curve.keyframe_points:
                        framelist.append({'frame': point.co[0], 'value': -PPI*point.co[1]})
                    keyframes['y'] = OrderedDict()
                    # keyframes['y']['fromKeyframeIndex'] = 0
                    # keyframes['y']['fromKeyframeIndexSaved'] = 0
                    keyframes['y']['datalist'] = sorted(framelist, key=itemgetter('frame'), reverse=False)
                    

            elif curve.data_path == 'rotation_euler':
                if axis == 2:#Z
                    framelist = []
                    for point in curve.keyframe_points:
                        framelist.append({'frame': point.co[0], 'value': -point.co[1]})
                    keyframes['rotation'] = OrderedDict()
                    # keyframes['rotation']['fromKeyframeIndex'] = 0
                    # keyframes['rotation']['fromKeyframeIndexSaved'] = 0
                    keyframes['rotation']['datalist'] = sorted(framelist, key=itemgetter('frame'), reverse=False)

            elif curve.data_path == 'scale':
                if axis == 0:#X
                    framelist = []
                    for point in curve.keyframe_points:
                        framelist.append({'frame': point.co[0], 'value': point.co[1]})
                    keyframes['scalex'] = OrderedDict()
                    # keyframes['scalex']['fromKeyframeIndex'] = 0
                    # keyframes['scalex']['fromKeyframeIndexSaved'] = 0
                    keyframes['scalex']['datalist'] = sorted(framelist, key=itemgetter('frame'), reverse=False)
                elif axis == 1:#Y
                    framelist = []
                    for point in curve.keyframe_points:
                        framelist.append({'frame': point.co[0], 'value': point.co[1]})
                    keyframes['scaley'] = OrderedDict()
                    # keyframes['scaley']['fromKeyframeIndex'] = 0
                    # keyframes['scaley']['fromKeyframeIndexSaved'] = 0
                    keyframes['scaley']['datalist'] = sorted(framelist, key=itemgetter('frame'), reverse=False)

        retunActionDict = OrderedDict()
        retunActionDict['BEG'] = action.frame_range[0]
        retunActionDict['END'] = action.frame_range[1]
        retunActionDict['keyframes'] = keyframes
        return retunActionDict
    #_________________________________________________________

    def get_fcurve_local_keyframes_list(self, obj, action):
        mat_pi = obj.matrix_parent_inverse
        pi_t = mat_pi.to_translation()
        pi_r = mat_pi.to_euler()
        pi_s = mat_pi.to_scale()

        keyframes = OrderedDict()
        for curve in action.fcurves:
            for point in curve.keyframe_points:
                keyframes[str(point.co[0])] = keyframes.get(str(point.co[0]),OrderedDict())
                keyframes[str(point.co[0])]['timestamp'] = point.co[0]
                axis = curve.array_index


                if curve.data_path == 'location':
                    if axis == 0:#X
                        keyframes[str(point.co[0])]['x'] = PPI*(point.co[1] - pi_t.x)
                    elif axis == 1:#Y
                        keyframes[str(point.co[0])]['y'] = -PPI*(point.co[1] - pi_t.y)
          
                elif curve.data_path == 'rotation_euler':
                    if axis == 2:#Z
                        keyframes[str(point.co[0])]['rotation'] = -(point.co[1] - pi_r.z)#zrotation is negative


                elif curve.data_path == 'scale':
                    if axis == 0:
                        if pi_s.x:
                            keyframes[str(point.co[0])]['scalex'] = point.co[1]/pi_s.x

                    elif axis == 1:
                        if pi_s.y:
                            keyframes[str(point.co[0])]['scaley'] = point.co[1]/pi_s.y

    
        keyframelist = [keyframes.get(f) for f in keyframes]
        # ldict[act.name] = sorted(keyframelist, key=itemgetter('timestamp'), reverse=False)
        return keyframelist
    #_________________________________________________________

    def write_blen2d(self, context, filepath):
        print('BLEN2D Export path: %r' % filepath)

        self.out_source_dir = os.path.dirname(bpy.data.filepath)
        self.out_dest_dir = os.path.dirname(filepath)
        file = open(filepath, "w", encoding="utf8", newline="\n")
        fw = file.write
      
        fw('// Blender v%s %r\n' % (bpy.app.version_string, os.path.basename(bpy.data.filepath)))
        fw('// www.blender.org\n')
        
        scene = bpy.context.scene
        _Os = bpy.data.objects
        HTML5assets = _Os.get('@Assets Library')
        HTML5root = _Os.get('@HTML5 root')
        HTML5properties = _Os.get('@Properties')
        fw('window.BLEN2D = window.BLEN2D || {}; window.BLEN2D.data = ')



        self.filedatadict[HTML5assets.name] = self.get_blend2d_dict(HTML5assets, 'assets')
        self.filedatadict[HTML5root.name] = self.get_blend2d_dict(HTML5root, 'objects')
        # self.filedatadict['@Actions'] = self.get_blend2d_dict(bpy.data.actions, 'actions')
        self.filedatadict[HTML5properties.name] = self.get_blend2d_dict(HTML5properties, 'props')
        

        fw(json.dumps(self.filedatadict, indent=4))

        

        # after the whole loop close the file
        file.close()
        return {'FINISHED'}

# ******************************************************************************************
# ******************************************************************************************

class Animation2DListItemScene(PropertyGroup):
    private_index = IntProperty(
            name="private_index",
            default = 1
    )
    obj = {}
    act = {}

# -----------------------------------------------------------------------------

class Animation2DSavedData(PropertyGroup):
    class Animation2DObjActBindingItem(PropertyGroup):
        savedobj = StringProperty(
               name="object",
               description="The object to which this action is associated",
               default="")
        savedact = StringProperty(
               name="action",
               description="The action which this object has for this animation",
               default="")


    strindex = StringProperty(
               name="stringkey",
               description="unque key to store animation in a global variable",
               default="")
    bindingdatalist = CollectionProperty(type = Animation2DObjActBindingItem)

# -----------------------------------------------------------------------------

class Animation2DListItem(PropertyGroup):
    name = StringProperty(
           name="Name",
           description="A name for this 2D Animation item",
           default="DefaultAnimation")

    index = IntProperty(
            name="index",
            default = 1
        )
    
# -----------------------------------------------------------------------------

class Animation2D_UI_List(UIList):
    def draw_item(self, context, layout, data, item, icon, active_data, active_propname, index):

        # We could write some code to decide which icon to use here...
        custom_icon = 'FREEZE'

        # Make sure your code supports all 3 layout types
        if self.layout_type in {'DEFAULT', 'COMPACT'}:
            layout.label(item.name, icon = custom_icon)

        elif self.layout_type in {'GRID'}:
            layout.alignment = 'CENTER'
            layout.label("", icon = custom_icon)

# -----------------------------------------------------------------------------

def init_Animation2D_property():
    bpy.types.Object.animation2D_list = CollectionProperty(type = Animation2DListItem)
    bpy.types.Object.animation2D_listINDEX = IntProperty(name = "Index for animation2D_list", default = 0)

    bpy.types.Scene.animation2D_list = CollectionProperty(type = Animation2DListItemScene)

    #Animation data to save in blend file
    bpy.types.Scene.animation2Dsaveddata = CollectionProperty(type = Animation2DSavedData)
    # bpy.types.Scene.animation2DObject_stringData = StringProperty(name="animationObjData")
    # bpy.types.Scene.animation2DAction_stringData = StringProperty(name="animationActData")
    # bpy.types.Scene.animation2DAnimation_stringData = StringProperty(name="animationAnimation2DData")
    bpy.types.Object.local_location = FloatVectorProperty(name="Location")
    bpy.types.Object.local_rotation = FloatVectorProperty(name="Rotation")
    bpy.types.Object.local_scale = FloatVectorProperty(name="Scale")

    bpy.types.Object.html5type =  EnumProperty(
                                        name="html5type",
                                        description="Choose the type of object it is",
                                        items=(('EMPTY', "EMPTY", "EMPTY"),
                                        ('MESH', "MESH", "MESH"),
                                        ('SPRITE', "SPRITE", "SPRITE"),
                                        ('BUTTON', "BUTTON", "BUTTON"),
                                        ),
                                        default='EMPTY')


    

    

    # load_animation_data_from_string(bpy.context.scene)

# ******************************************************************************************
# ******************************************************************************************
from bpy.app.handlers import persistent

def save_animation_data_to_string(scene):
    if len(scene.animation2D_list):
        sceneanim = scene.animation2D_list[0]
        # scene.animation2DObject_stringData = str(sceneanim.obj)
        # scene.animation2DAction_stringData = str(sceneanim.act)
        # scene.animation2DAnimation_stringData = str(sceneanim.animation)

        scene.animation2Dsaveddata.clear()
        idx = -1
        for animskey in sceneanim.obj:
            scene.animation2Dsaveddata.add()
            idx+=1
            objlist = sceneanim.obj[animskey]
            actlist = sceneanim.act[animskey]
            idxx = -1
            for i in range(len(objlist)):
                if objlist[i] and actlist[i]:
                    scene.animation2Dsaveddata[idx].bindingdatalist.add()
                    idxx+=1
                    scene.animation2Dsaveddata[idx].strindex = animskey
                    scene.animation2Dsaveddata[idx].bindingdatalist[idxx].savedobj = str([objlist[i]])
                    scene.animation2Dsaveddata[idx].bindingdatalist[idxx].savedact = str([actlist[i]])

# -----------------------------------------------------------------------------

def load_animation_data_from_string(scene):
    if len(scene.animation2D_list) < 1:
        scene.animation2D_list.add()
    if len(scene.animation2D_list):
        sceneanim = scene.animation2D_list[0]

        for an in scene.animation2Dsaveddata:
            objbindlist = []
            actbindlist = []
            for binddata in an.bindingdatalist:
                try:
                    oneobj = eval(binddata.savedobj)[0]
                except KeyError:
                    oneobj = None

                try:
                    oneact = eval(binddata.savedact)[0]
                except KeyError:
                    oneact = None

                if oneobj and oneact:
                    objbindlist.append(oneobj)
                    actbindlist.append(oneact)

            if(len(objbindlist) and len(actbindlist)):
                sceneanim.obj[an.strindex] = objbindlist
                sceneanim.act[an.strindex] = actbindlist

# -----------------------------------------------------------------------------

@persistent
def save_pre_handler(scene):
    scene = bpy.context.scene
    print("Save pre Handler", bpy.data.filepath)
    save_animation_data_to_string(scene)
bpy.app.handlers.save_pre.append(save_pre_handler)
# ---------------------------------------------------------
@persistent
def load_post_handler(scene):
    scene = bpy.context.scene
    print("Load post Handler", bpy.data.filepath)
    load_animation_data_from_string(scene)
bpy.app.handlers.load_post.append(load_post_handler)

# bpy.app.handlers.scene_update_post.append(update_blen2d_local_transform)

# ******************************************************************************************
# ******************************************************************************************

# registering and menu integration
def register():
    bpy.utils.register_module(__name__)
    bpy.types.INFO_MT_file_import.append(import_images_button)
    bpy.types.INFO_MT_mesh_add.append(import_images_button)

    bpy.types.INFO_MT_file_export.append(export_blen2d_button)

    init_Animation2D_property()


# -----------------------------------------------------------------------------    

# unregistering and removing menus
def unregister():
    bpy.utils.unregister_module(__name__)
    bpy.types.INFO_MT_file_import.remove(import_images_button)
    bpy.types.INFO_MT_mesh_add.remove(import_images_button)

    bpy.types.INFO_MT_file_export.remove(export_blen2d_button)
  
# -----------------------------------------------------------------------------

if __name__ == "__main__":
    register()
