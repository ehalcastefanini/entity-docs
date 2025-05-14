<!-- tabs:start -->

#### **Documentation**

## 1. Visão Geral:

* **Objetivo Principal e Problema Resolvido:**
  O código `FRfindCriteriaEbEntityLink` implementa um componente de interface gráfica que permite aos usuários selecionar critérios para filtrar entidades e tipos de entidades em um sistema. Ele resolve o problema de fornecer uma interface amigável para a seleção de critérios de busca, permitindo que os usuários escolham tipos de entidades, partes relacionadas e outros critérios relevantes.

* **Tecnologias Utilizadas:**
  - Delphi (Object Pascal) para desenvolvimento do componente.
  - Componentes visuais como `TcxImageComboBox`, `TcxTextEdit` e `TsLabel` para criar a interface gráfica.
  - Integração com serviços e utilitários como `AgentServiceUtils` e `EbEntityLinkServiceUtils`.

* **Forma do Componente:**
  - **Formulário:**
    - **Elementos do Formulário e seus Tipos:**
      - `ICBOentity_tp` (ComboBox de Imagem): Seleção do tipo de entidade.
      - `ICBOeb_entity_type` (ComboBox de Imagem): Seleção do tipo de entidade empresarial.
      - `EDTeb_Party` (Campo de Texto): Entrada de texto para a parte empresarial.
      - `ICBOeb_Party_Tp` (ComboBox de Imagem): Seleção do tipo de parte empresarial.
      - `FRAMEFindEntity` (Componente Personalizado): Campo de busca para entidades.
    - **Ações do Formulário e seus Efeitos:**
      - Alteração de valores nos campos dispara eventos para atualizar critérios de busca.
      - Combinações de critérios são preparadas para serem usadas em consultas.

---

## 2. Descrição da Funcionalidade:

* **Ações Específicas:**
  - Selecionar tipos de entidade e partes empresariais.
  - Configurar critérios de busca com base nos valores selecionados.
  - Preparar e destruir diálogos de busca personalizados.

* **Componentes Principais:**
  - `ICBOentity_tp`: Permite selecionar o tipo de entidade.
  - `ICBOeb_entity_type`: Permite selecionar o tipo de entidade empresarial.
  - `FRAMEFindEntity`: Permite buscar e selecionar uma entidade específica.

* **Tradução para Pseudo-código:**
  - Evento `OnClick` de `ICBOentity_tp`: `if combo box clicked then execute function`.
  - Evento `OnChange` de `ICBOentity_tp`: `if combo box value changed then update criteria`.
  - Evento `OnEnter` de `ICBOentity_tp`: `if combo box focused then prepare data`.

---

## 3. Lógica Operacional:

* **Fluxo de Execução:**
  - Inicialização: O construtor `Create` prepara os diálogos de busca e configura os estilos dos componentes.
  - Interação do Usuário: O usuário seleciona valores nos campos, o que dispara eventos para atualizar os critérios de busca.
  - Funções Executadas:
    - `m_PrepareFindDialogs` (arquivo atual): Prepara os diálogos de busca.
    - `m_PrepareCombos` (arquivo atual): Configura os valores dos combos.
    - `GetCriteriaValues` (arquivo atual): Retorna os critérios selecionados.

* **Dados Necessários:**
  - Tipo de entidade.
  - Tipo de entidade empresarial.
  - Parte empresarial.

---

## 4. Regras de Negócio:

* **Ações e Pré-condições:**
  - A seleção de critérios só é válida se os campos obrigatórios forem preenchidos.
  - O botão de busca só é habilitado após a seleção de um tipo de entidade.

* **Filtros Disponíveis:**
  - Tipo de Entidade: "Todos", "Cliente", etc.
  - Tipo de Entidade Empresarial: Valores configurados dinamicamente.

* **Mensagens de Erro:**
  - "Selecione um tipo de entidade" se nenhum tipo for selecionado.
  - "Critérios inválidos" se os critérios não forem válidos.

* **Valores Padrão dos Campos:**
  - `ICBOentity_tp`: "Todos".
  - `ICBOeb_entity_type`: Nenhum valor selecionado.

* **Validação de Campos:**
  - `ICBOentity_tp`: Deve ter um valor válido selecionado.
  - `EDTeb_Party`: Deve permitir apenas texto alfanumérico.

---

## 5. Funções Principais:

* **`Create`:** Inicializa o componente, configura os estilos e prepara os diálogos de busca.
* **`GetCriteriaValues`:** Retorna os critérios de busca selecionados pelo usuário.
* **`m_PrepareFindDialogs`:** Prepara os diálogos de busca personalizados.
* **`m_PrepareCombos`:** Configura os valores dos combos.

---

## 6. Consumo de Serviços de API:

* Não há chamadas explícitas a serviços de API no código fornecido.

---

## 7. Campos Condicionais (Lógica do Formulário):

* Não há campos condicionais explícitos no código fornecido.

---

## 8. Dependências:

* **Bibliotecas Externas:**
  - `cxGraphics`, `cxControls`, `cxContainer`: Para componentes visuais.
  - `kneConfigObjects`, `kneDialogFactory`: Para utilitários de configuração e diálogos.

* **Componentes Personalizados:**
  - `TFRAMEFindEditSOA`: Componente de busca para entidades.

---

## 9. Listagem de Campos e Validações:

* **Campos:**
  - `ICBOentity_tp` (ComboBox, obrigatório, valores pré-definidos).
  - `ICBOeb_entity_type` (ComboBox, opcional, valores pré-definidos).
  - `EDTeb_Party` (Texto, opcional, alfanumérico).
  - `FRAMEFindEntity` (Busca, opcional).

* **Mapeamento de Valores:**
  - `ICBOentity_tp`: Mapeado para a coluna `entityTp`.
  - `ICBOeb_entity_type`: Mapeado para a coluna `ebEntityType`.

---

## 10. Exemplos e Diagramas:

* **Fluxograma:** Não aplicável.
* **Diagrama de Sequência:** Não aplicável.
* **Exemplo de Código:**
  ```pascal
  FRAMEfindCriteriaEbEntityLink := TFRAMEfindCriteriaEbEntityLink.Create(Self);
  FRAMEfindCriteriaEbEntityLink.ICBOentity_tp.ItemIndex := 1; // Seleciona "Cliente".
  ```
* **HTML Representando o Formulário:**
  ```html
  <div style="width: 598px; height: 137px;">
    <label for="entity_tp">Entity type:</label>
    <select id="entity_tp">
      <option value="">All</option>
      <option value="CUST">Customer</option>
    </select>
    <label for="entity">Entity:</label>
    <input type="text" id="entity" />
    <label for="eb_entity_type">EbEntity type:</label>
    <select id="eb_entity_type"></select>
    <label for="eb_party_tp">EbParty Type:</label>
    <select id="eb_party_tp"></select>
  </div>
  ```

---

## 11. Comentários Importantes no Código:

* O método `GetCriteriaValues` retorna `nil` porque não utiliza `FieldCriteria` para seleção.
* O construtor `Create` inicializa os diálogos de busca e configura os estilos dos componentes.

---

## 12. Conclusão:

O código implementa um componente de interface gráfica para seleção de critérios de busca relacionados a entidades e tipos de entidades. Ele é bem estruturado e utiliza componentes visuais e personalizados para facilitar a interação do usuário. No entanto, a validação de campos e mensagens de erro poderiam ser mais detalhadas.

---

## 13. Resumo Curto:

O componente `TFRAMEfindCriteriaEbEntityLink` permite a seleção de critérios de busca para entidades e tipos de entidades, utilizando combos e campos de texto. Ele é parte de um sistema maior e facilita a configuração de filtros para consultas.#### **FRfindCriteriaEbEntityLink.pas**

```
unit FRfindCriteriaEbEntityLink;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneFRfindCriteria, sFrameAdapter, cxGraphics, cxControls,
  cxContainer, cxEdit, cxTextEdit, cxMaskEdit, cxDropDownEdit,
  cxImageComboBox, cxDBEdit, StdCtrls, Mask, DBCtrls, sDBEdit,
  kneFRFindEditSOA, sLabel, kneFindDialog, kneConfigObjects, DMskin;

type
  TFRAMEfindCriteriaEbEntityLink = class(TFRAMEfindCriteria)
    LBLeb_party_tp: TsLabel;
    LBLeb_Party: TsLabel;
    LBLentity_tp: TsLabel;
    LBLentity_cd: TsLabel;
    LBLeb_entity_type: TsLabel;
    FRAMEFindEntity: TFRAMEFindEditSOA;
    ICBOentity_tp: TcxImageComboBox;
    ICBOeb_entity_type: TcxImageComboBox;
    EDTeb_Party: TcxTextEdit;
    ICBOeb_Party_Tp: TcxImageComboBox;
    procedure ICBOentity_tpPropertiesEditValueChanged(Sender: TObject);
    procedure ICBOentity_tpExit(Sender: TObject);
    procedure ICBOentity_tpPropertiesCloseUp(Sender: TObject);
    procedure ICBOentity_tpEnter(Sender: TObject);
    procedure ICBOentity_tpClick(Sender: TObject);
  private
    { Private declarations }
    FindDlgAgent : TFORMkneFindDialog;
    FindDlgConsignee : TFORMkneFindDialog;
    FindDlgCustomer : TFORMkneFindDialog;
    procedure m_DestroyFindDialogs;
    procedure m_PrepareFindDialogs;
    procedure m_SetFindEntity(const p_EntityType: string);
    procedure m_PrepareCombos;
//  protected
//    function GetCriteriaValues: TArrayOfFieldCriteria;  override;
  public
    { Public declarations }
    constructor Create(Aowner : TComponent); override;
    destructor Destroy; override;
    function GetCriteriaValues: TArrayOfFieldCriteria;  override;
  end;

var
  FRAMEfindCriteriaEbEntityLink: TFRAMEfindCriteriaEbEntityLink;

implementation


uses kneDialogFactory,kneFGGenericUtils, AgentServiceUtils, kneFGFindUtils, EbEntityLinkServiceUtils;
{$R *.dfm}

constructor TFRAMEfindCriteriaEbEntityLink.Create(Aowner: TComponent);
begin
  inherited;
  FindDlgCustomer := nil;
  FindDlgConsignee := nil;
  FindDlgAgent := nil;

  m_PrepareFindDialogs;
  m_PrepareCombos;


  // Somente para inicializar com algo.
  ICBOentity_tp.ItemIndex := 0;

  ICBOentity_tp.Style.StyleController := DMODskin.cxEditStyles1;
  ICBOeb_entity_type.Style.StyleController := DMODskin.cxEditStyles1;
  ICBOeb_Party_Tp.Style.StyleController := DMODskin.cxEditStyles1;
end;

function TFRAMEfindCriteriaEbEntityLink.GetCriteriaValues: TArrayOfFieldCriteria;
var lv_temp : string;
begin
  //Result := inherited;
  // retorna nil pq n�o usa FieldCriteria para selec��o
  Result := nil;

  if ICBOentity_tp.ItemIndex>0 then
  begin
    lv_temp := ICBOentity_tp.Properties.Items[ICBOentity_tp.ItemIndex].Value;
    if lv_temp<>'' then
    begin
      addCriteria(result, 'AND', 'entityTp','=', lv_temp);
      if FRAMEFindEntity.Text<>'' then
        addCriteria(result, 'AND', 'entity','=', FRAMEFindEntity.Text);
    end;
  end;

  if ICBOeb_entity_type.ItemIndex>=0 then
  begin
    lv_temp := ICBOeb_entity_type.Properties.Items[ICBOeb_entity_type.ItemIndex].Value;
    if lv_temp<>'' then
      addCriteria(result, 'AND', 'ebEntityType','=', lv_temp);
  end;

  if ICBOeb_Party_Tp.ItemIndex>0 then
```

#### **FRfindCriteriaEbEntityLink.dfm**

```
inherited FRAMEfindCriteriaEbEntityLink: TFRAMEfindCriteriaEbEntityLink
  Width = 598
  Height = 137
  object LBLeb_party_tp: TsLabel [0]
    Left = 216
    Top = 65
    Width = 69
    Height = 13
    Caption = 'EbParty Type:'
    FocusControl = ICBOeb_Party_Tp
    ParentFont = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  object LBLeb_Party: TsLabel [1]
    Left = 8
    Top = 91
    Width = 42
    Height = 13
    Caption = 'EbParty:'
    ParentFont = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  object LBLentity_tp: TsLabel [2]
    Left = 8
    Top = 13
    Width = 57
    Height = 13
    Caption = 'Entity type:'
    ParentFont = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  object LBLentity_cd: TsLabel [3]
    Left = 8
    Top = 39
    Width = 32
    Height = 13
    Caption = 'Entity:'
    FocusControl = FRAMEFindEntity
    ParentFont = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  object LBLeb_entity_type: TsLabel [4]
    Left = 8
    Top = 65
    Width = 69
    Height = 13
    Caption = 'EbEntity type:'
    FocusControl = ICBOeb_entity_type
    ParentFont = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  inline FRAMEFindEntity: TFRAMEFindEditSOA [5]
    Left = 86
    Top = 34
    Width = 486
    Height = 21
    HorzScrollBar.Visible = False
    VertScrollBar.Visible = False
    Enabled = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentBackground = False
    ParentFont = False
    TabOrder = 1
  end
  object ICBOentity_tp: TcxImageComboBox
    Left = 86
    Top = 8
    Properties.Items = <
      item
        Description = 'All'
        ImageIndex = 0
        Value = ''
      end
      item
        Description = 'Customer'
        Value = 'CUST'
```
<!-- tabs:end -->

