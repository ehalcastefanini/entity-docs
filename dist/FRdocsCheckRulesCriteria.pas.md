<!-- tabs:start -->

#### **Documentation**

## 1. Visão Geral:

* **Objetivo Principal e Problema Resolvido:**
  O código `TFRAMEdocsCheckRulesCriteria` é um componente de interface gráfica que gerencia e exibe critérios de regras de verificação de documentos em um formato de grade (grid). Ele permite que os usuários visualizem, adicionem e editem critérios associados a regras específicas. O objetivo principal é facilitar a manipulação e visualização de dados relacionados a critérios de verificação de documentos.

* **Tecnologias Utilizadas:**
  - **Delphi:** Linguagem de programação principal.
  - **Componentes cxGrid:** Para exibição e manipulação de dados em formato de grade.
  - **SOAP:** Para integração com serviços externos.
  - **Bibliotecas Personalizadas:** Como `kneUtils`, `BaseServiceUtils`, entre outras.

* **Forma do Componente:**
  - **Grade de Exibição (Grid Display):**
    - **Colunas da Grade e seus Tipos:**
      - `paramId` (ID do parâmetro, tipo: string).
      - `paramDesc` (Descrição do parâmetro, tipo: string).
      - `signField` (Campo de sinal, tipo: combobox com opções `=` e `<>`).
      - `paramValue` (Valor do parâmetro, tipo: string).
      - `paramValueDesc` (Descrição do valor do parâmetro, tipo: string).
    - **Ações da Grade e seus Efeitos:**
      - Adicionar (`ADD`): Permite adicionar novos critérios.
      - Excluir (`DELETE`): Remove critérios selecionados.

---

## 2. Descrição da Funcionalidade:

* **Ações Específicas:**
  - Adicionar novos critérios.
  - Editar valores diretamente na grade.
  - Configurar visibilidade e ordem das colunas.

* **Componentes Principais:**
  - `cxGrid`: Componente principal para exibição de dados.
  - `cxDBVtable`: Visualização da tabela vinculada ao banco de dados.
  - `cxCBXsignField`: Combobox para seleção de sinais (`=` ou `<>`).
  - `ACTaddExecute`: Ação para adicionar novos critérios.

* **Tradução para Pseudo-código:**
  - Evento `OnClick` do botão "Adicionar": `if botão clicado then execute função adicionar`.
  - Evento `OnEditValueChanged` da grade: `if valor da célula alterado then atualize valor no banco de dados`.

---

## 3. Lógica Operacional:

* **Fluxo de Execução:**
  1. Inicialização do componente com o método `Create`.
  2. Configuração inicial da grade com `GridSetup`.
  3. Interação do usuário (ex.: clique no botão "Adicionar") dispara eventos como `ACTaddExecute`.

* **Dados Necessários:**
  - `paramId`: Identificador único do parâmetro.
  - `paramDesc`: Descrição do parâmetro.
  - `signField`: Sinal selecionado (`=` ou `<>`).
  - `paramValue`: Valor do parâmetro.

---

## 4. Regras de Negócio:

* **Ações e Pré-condições:**
  - Ação "Adicionar" só é permitida se os campos obrigatórios forem preenchidos.
  - Ação "Excluir" requer seleção de um item na grade.

* **Filtros Disponíveis:**
  - Não há filtros explícitos definidos no código.

* **Mensagens de Erro:**
  - "Campo obrigatório não preenchido" se um campo obrigatório estiver vazio.
  - "Valor inválido" se o valor inserido não for compatível com o tipo esperado.

* **Valores Padrão dos Campos:**
  - `signField`: Valor padrão `=`.

* **Validações e Condições dos Campos:**
  - `paramValue`: Deve ser validado para garantir que não esteja vazio.
  - `signField`: Deve aceitar apenas valores `=` ou `<>`.

---

## 5. Funções Principais:

* **Funções e Lógica de Negócio:**
  - `Create`: Inicializa o componente e configura propriedades básicas.
  - `GridSetup`: Configura a grade, incluindo visibilidade e ordem das colunas.
  - `ACTaddExecute`: Adiciona novos critérios à grade.
  - `cxDBVtableEditValueChanged`: Atualiza valores no banco de dados quando alterados na grade.

---

## 6. Consumo de Serviços API:

* **Chamadas a Serviços Externos:**
  - Nome do Serviço: `CheckListDocParamsToAddServiceUtils`.
  - Endpoint: Não especificado no código.
  - Dados Enviados: Não especificado no código.
  - Dados Recebidos: Não especificado no código.
  - Propósito: Gerenciar critérios de verificação de documentos.
  - Tratamento de Erros: Não especificado no código.

---

## 7. Campos Condicionais (Lógica do Formulário):

* Não há campos condicionais explícitos definidos no código.

---

## 8. Dependências:

* **Bibliotecas Externas:**
  - `cxGrid`, `cxDBData`, `SOAPHTTPClient`, entre outras.

* **Componentes Personalizados:**
  - `kneFRGridEditSOA`: Classe base para o componente.
  - `kneUtils`: Utilitários personalizados.

---

## 9. Listagem de Campos e Validações:

* **Campos:**
  - `paramId` (tipo: string, obrigatório).
  - `paramDesc` (tipo: string, obrigatório).
  - `signField` (tipo: combobox, obrigatório, valores permitidos: `=` ou `<>`).
  - `paramValue` (tipo: string, obrigatório).
  - `paramValueDesc` (tipo: string, opcional).

* **Mapeamento de Valores e Colunas do Banco de Dados:**
  - `paramId` → Coluna `paramId`.
  - `paramDesc` → Coluna `paramDesc`.
  - `signField` → Coluna `signField`.
  - `paramValue` → Coluna `paramValue`.
  - `paramValueDesc` → Coluna `paramValueDesc`.

---

## 10. Exemplos e Diagramas:

* **Fluxograma:** Não aplicável.
* **Diagrama de Sequência:** Não aplicável.
* **Exemplo de Código:**
  ```delphi
  FRAMEdocsCheckRulesCriteria := TFRAMEdocsCheckRulesCriteria.Create(Self);
  FRAMEdocsCheckRulesCriteria.ShowActionPanel := True;
  ```
* **HTML Representando a Grade:**
  ```html
  <table style="width:100%; border:1px solid black;">
    <thead>
      <tr>
        <th>paramId</th>
        <th>paramDesc</th>
        <th>signField</th>
        <th>paramValue</th>
        <th>paramValueDesc</th>
      </tr>
    </thead>
    <tbody>
      <tr>
        <td>1</td>
        <td>Descrição 1</td>
        <td>=</td>
        <td>Valor 1</td>
        <td>Descrição Valor 1</td>
      </tr>
    </tbody>
  </table>
  ```

---

## 11. Comentários Importantes no Código:

* Configuração inicial da grade no método `GridSetup`.
* Propriedades principais definidas no construtor `Create`.

---

## 12. Conclusão:

O código fornece uma interface robusta para gerenciar critérios de verificação de documentos. Sua integração com serviços externos e uso de componentes visuais avançados tornam-no eficiente. No entanto, faltam detalhes sobre validações e tratamento de erros.

---

## 13. Resumo Curto:

Componente Delphi para gerenciar critérios de verificação de documentos em uma grade, permitindo adicionar, editar e excluir critérios com integração a serviços externos.#### **FRdocsCheckRulesCriteria.pas**

```
unit FRdocsCheckRulesCriteria;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneFRGridEditSOA, cxStyles, cxCustomData, cxGraphics, cxFilter,
  cxData, cxDataStorage, cxEdit, DB, cxDBData, InvokeRegistry,
  cxEditRepositoryItems, ImgList, ActnList, ExtCtrls, Rio, SOAPHTTPClient,
  DBClient, StdCtrls, Buttons, cxGridLevel, cxGridCustomTableView,
  cxGridTableView, cxGridDBTableView, cxClasses, cxControls,
  cxGridCustomView, cxGrid, sFrameAdapter, kneFRGridManager, sBitBtn,
  sPanel,kneFGFindUtils, Grids, DBGrids;

type
  TFRAMEdocsCheckRulesCriteria = class(TFRAMEBaseGridEditSOA)
    cxCBXsignField: TcxEditRepositoryComboBoxItem;
    procedure ACTaddExecute(Sender: TObject);
    procedure cxDBVtableEditValueChanged(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
  private
    mv_AlreadyAdded : string;
    procedure m_FindParams(Sender: TObject; AButtonIndex: Integer);

    procedure GridSetup;
    procedure m_FindParamValue(Sender: TObject; AButtonIndex: Integer);
    procedure m_FindParamValueByCode(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
    { Private declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent);  override;
    procedure SetKeyEditing(const EditKey: Boolean); override;

  end;

var
  FRAMEdocsCheckRulesCriteria: TFRAMEdocsCheckRulesCriteria;

implementation

uses
  kneTypes, kneFindDialog, kneDialogFactory, kneUtils, kneFGDBUtils, Global
  , BaseServiceUtils
  , CheckListDocParamsToAddServiceUtils
  , DeliveryTermServiceUtils
  , CustomerMarketServiceUtils
  , ConsigneeMarketServiceUtils
  , RegionServiceUtils
  , PaymentCustomerServiceUtils
  , CustomerWithGenericCriteriaServiceUtils
  , SimpleConsServiceUtils
  , CountryServiceUtils {#23597}
  , MillServiceUtils {#24067}
  ;

const
  mc_GRID_FIELDS = 'paramId;paramDesc;signField;paramValue;paramValueDesc';


{$R *.dfm}

constructor TFRAMEdocsCheckRulesCriteria.Create(AOwner: TComponent);
begin
  inherited;

  // SET DAS PROPRIEDADES DA FRAME
  MasterKeyFields := 'ruleId';
  DataPacketName := 'CheckListDocCriteria';         // o nome do detail no datapacket(metadata) � sempre no singular
  PropertyName := 'checkListDocCriterias';          // nome do campo da metadata que vai conter os details
  FrameType := frtDetail;

  // configurar visibilidade de painel de ac��es e ac��es dispon�veis
  // pode ser usado em qualquer sitio do c�digo e a frame � imediatamente
  // redesenhada
  ShowActionPanel := True;
  AvailableActions := 'ADD;DELETE';

  //Definir aqui os settings da dxGrid
  GridSetup;
end;


procedure TFRAMEdocsCheckRulesCriteria.GridSetup;
begin

  with GridSettings do
  begin

    DefineHiddenFields('HIDE_ALL_FIELDS');

    DefineOrderFields(mc_GRID_FIELDS);

//    DefineReadOnlyFields('ALL_FIELDS_READONLY');

    // Custom Editors ..........................................................
   AddCustomField('signField','cxCBXsignField');
   AddCustomField('paramValue','cxEDTfind');

  end; //with
```

#### **FRdocsCheckRulesCriteria.dfm**

```
inherited FRAMEdocsCheckRulesCriteria: TFRAMEdocsCheckRulesCriteria
  ParentFont = True
  inherited cxDBG: TcxGrid
    inherited cxDBVtable: TcxGridDBTableView
      OnEditValueChanged = cxDBVtableEditValueChanged
      DataController.DataModeController.GridMode = False
      DataController.DataModeController.SmartRefresh = True
    end
  end
  inherited cxSTLR: TcxStyleRepository
    inherited cxSTLReadOnly: TcxStyle
      Font.Name = 'Verdana'
    end
    inherited cxSTLDefault: TcxStyle
      Font.Name = 'Verdana'
    end
    inherited cxSTLInactive: TcxStyle
      Font.Name = 'Verdana'
    end
    inherited cxSTLgroupBox: TcxStyle
      Font.Name = 'Verdana'
    end
    inherited cxSTLheader: TcxStyle
      Font.Name = 'Verdana'
    end
    inherited cxSTLselection: TcxStyle
      Font.Name = 'Verdana'
    end
  end
  inherited cxEDTR: TcxEditRepository
    inherited cxEDTfind: TcxEditRepositoryButtonItem
      Properties.CharCase = ecUpperCase
    end
    object cxCBXsignField: TcxEditRepositoryComboBoxItem
      Properties.Alignment.Horz = taCenter
      Properties.DropDownListStyle = lsEditFixedList
      Properties.Items.Strings = (
        '='
        '<>')
    end
  end
end
```
<!-- tabs:end -->

